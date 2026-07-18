// Smoke test for the headless export API, run under node (bare V8, no DOM).
// Under CommonJS, js_of_ocaml attaches Js.export to module.exports; in a plain
// engine (ClearScript) the same export lands on globalThis.tryhept.
const t = require(process.cwd() + "/tryhept.js").tryhept;

function fail(msg) { console.error("SMOKE FAIL: " + msg); process.exit(1); }
function eq(a, b, what) {
  if (JSON.stringify(a) !== JSON.stringify(b)) fail(`${what}: expected ${JSON.stringify(b)}, got ${JSON.stringify(a)}`);
}

eq(t.version(), "clearscript-export-1", "version");

const src = `node gate_counter() returns (last cnt : int = 0; gate : bool)
let
  automaton
    state Counting
      do cnt = last cnt + 1;
         gate = false
      until cnt >= 3 then Waiting
    state Waiting
      do cnt = 0;
         gate = true
      until true then Counting
  end
tel

node counter() returns (cnt : int; gate : bool)
let
  cnt = (0 fby (cnt + 1)) % 4;
  gate = (cnt = 0);
tel
`;

const r = JSON.parse(t.compile(src));
if (!r.ok) fail("compile failed: " + r.diagnostics);
eq(r.nodes.map(n => n.name).sort(), ["counter", "gate_counter"], "node names");

const oracle = {
  gate_counter: [[1,false],[2,false],[3,false],[0,true],[1,false],[2,false],[3,false],[0,true]],
  counter:      [[0,true],[1,false],[2,false],[3,false],[0,true],[1,false],[2,false],[3,false]],
};

for (const name of Object.keys(oracle)) {
  const h = t.instantiate(name);
  if (h < 0) fail("instantiate " + name);
  const run = () => Array.from({length: 8}, () => JSON.parse(t.step(h, "[]")));
  eq(run(), oracle[name], name + " ticks");
  t.reset(h);
  eq(run(), oracle[name], name + " ticks after reset");
}

const bad = JSON.parse(t.compile("node broken() returns (x : int)\nlet\n  x = y + 1;\ntel\n"));
if (bad.ok) fail("broken program compiled");
if (!/line \d+/.test(bad.diagnostics)) fail("no located diagnostic: " + bad.diagnostics);

console.log("SMOKE OK");
