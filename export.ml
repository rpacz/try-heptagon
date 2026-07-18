(** Headless Js.export API so the compiler+interpreter bundle can be driven
    from a bare JS engine (ClearScript/V8) with no DOM. Structured data crosses
    the boundary as JSON strings.

    Exported as globalThis.tryhept:
      version                     -> string
      compile(src)                -> json {ok, diagnostics, nodes:[{name, inputs:[{name,type}], outputs:[...]}]}
      instantiate(nodeName)       -> int handle (-1 on error; see diagnostics of next compile call)
      reset(handle)               -> unit
      step(handle, inputsJson)    -> json [outputs...] or {error}

    Linked into tryhept.byte via the Export.init () call in tryhept.ml — an
    unreferenced module would not be linked by ocamlbuild at all. *)

open Js_of_ocaml
open Obc

let diag = Buffer.create 1024

let compiled : Obc.program option ref = ref None

(* handle -> (interpreter, step-method input types) *)
let instances : (int, (module Simul.Interpreter) * Types.ty list) Hashtbl.t = Hashtbl.create 8
let next = ref 0

let js_str s = Js.Unsafe.inject (Js.string s)

let json_parse (s : Js.js_string Js.t) : Js.Unsafe.any =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.parse") [| Js.Unsafe.inject s |]

let json_stringify (v : Js.Unsafe.any) : Js.js_string Js.t =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "JSON.stringify") [| v |]

let rec type_name (t : Types.ty) : string =
  match t with
  | Types.Tid q -> q.Names.name
  | Types.Tarray (t, _) -> type_name t ^ "[]"
  | Types.Tprod _ -> "prod"
  | Types.Tinvalid -> "invalid"

let port_obj name ty =
  Js.Unsafe.obj [| ("name", js_str name); ("type", js_str (type_name ty)) |]

let ports_of vds =
  Js.Unsafe.inject
    (Js.array (Array.of_list
      (List.map (fun vd -> port_obj (Idents.source_name vd.v_ident) vd.v_type) vds)))

let sig_obj (cd : Obc.class_def) =
  let met = Obc_interp.find_method cd Mstep in
  Js.Unsafe.obj [|
    ("name", js_str cd.cd_name.Names.name);
    ("inputs", ports_of met.m_inputs);
    ("outputs", ports_of met.m_outputs);
  |]

let compile_impl (src : string) : Js.Unsafe.any =
  Buffer.clear diag;
  try
    (* Mirror the app: parse+check for diagnostics, then a fresh env for codegen *)
    let modname = Compil.prepare_module () in
    let p = Compil.parse_program modname src in
    Compil.check_program p stdout;
    let modname = Compil.prepare_module () in
    let p = Compil.parse_program modname src in
    let p = Compil.compile_program modname p in
    compiled := Some p;
    Hashtbl.reset instances;
    let sigs =
      List.filter_map
        (function
          | Pclass cd -> (try Some (sig_obj cd) with _ -> None)
          | _ -> None)
        p.p_desc
    in
    Js.Unsafe.obj [|
      ("ok", Js.Unsafe.inject Js._true);
      ("diagnostics", js_str (Buffer.contents diag));
      ("nodes", Js.Unsafe.inject (Js.array (Array.of_list sigs)));
    |]
  with e ->
    (match e with
     | Errors.Error -> ()   (* message already on stderr -> diag *)
     | e -> Buffer.add_string diag (Printexc.to_string e));
    Js.Unsafe.obj [|
      ("ok", Js.Unsafe.inject Js._false);
      ("diagnostics", js_str (Buffer.contents diag));
      ("nodes", Js.Unsafe.inject (Js.array [||]));
    |]

let instantiate_impl (name : string) : int =
  match !compiled with
  | None -> Buffer.add_string diag "instantiate: no compiled program\n"; -1
  | Some p ->
    (try
       let cd = Obc_interp.find_class p name in
       let met = Obc_interp.find_method cd Mstep in
       let in_tys = List.map (fun vd -> vd.v_type) met.m_inputs in
       let module I = Interp.ObcInterpreter(struct let prog = p let classname = name end) in
       I.reset ();
       incr next;
       Hashtbl.replace instances !next ((module I : Simul.Interpreter), in_tys);
       !next
     with e -> Buffer.add_string diag (Printexc.to_string e); -1)

let reset_impl (h : int) : unit =
  match Hashtbl.find_opt instances h with
  | Some ((module I : Simul.Interpreter), _) -> I.reset ()
  | None -> ()

let step_impl (h : int) (ins_json : Js.js_string Js.t) : Js.js_string Js.t =
  try
    let (module I : Simul.Interpreter), in_tys =
      match Hashtbl.find_opt instances h with
      | Some x -> x
      | None -> failwith (Printf.sprintf "step: unknown handle %d" h)
    in
    let arr = Js.to_array (Js.Unsafe.coerce (json_parse ins_json)) in
    let expected = List.length in_tys and got = Array.length arr in
    if got <> expected then
      failwith (Printf.sprintf "step: expected %d inputs, got %d" expected got);
    let ins = List.mapi (fun i ty -> Js_obc_conversion.obc_of_js ty arr.(i)) in_tys in
    let outs = I.step ins in
    json_stringify
      (Js.Unsafe.inject (Js.array (Array.of_list (List.map Js_obc_conversion.js_of_obc outs))))
  with e ->
    json_stringify (Js.Unsafe.obj [| ("error", js_str (Printexc.to_string e)) |])

let write_stdlib () =
  let w name bytes =
    let outf = open_out_bin name in
    List.iter (output_byte outf) bytes;
    close_out outf
  in
  w "pervasives.epci" Pervasives.pervasives;
  w "mathlib.epci" Mathlib.mathlib

let init () =
  (* In the browser the app installs its own stderr flusher afterwards and wins *)
  Sys_js.set_channel_flusher stderr (fun s -> Buffer.add_string diag s);
  Sys_js.set_channel_flusher stdout (fun _ -> ());
  write_stdlib ();
  Js.export "tryhept"
    (object%js
       method version = Js.string "clearscript-export-1"
       method compile (src : Js.js_string Js.t) =
         json_stringify (compile_impl (Js.to_string src))
       method instantiate (name : Js.js_string Js.t) =
         instantiate_impl (Js.to_string name)
       method reset (h : int) = reset_impl h
       method step (h : int) (ins : Js.js_string Js.t) = step_impl h ins
     end)
