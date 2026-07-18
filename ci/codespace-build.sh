#!/usr/bin/env bash
# One-shot build of tryhept.js inside a GitHub Codespace (Ubuntu universal image).
# Mirrors the README build; every step is logged so failures are diagnosable remotely.
set -uxo pipefail

cd "$(dirname "$0")/.."

sudo apt-get update -y
sudo apt-get install -y opam

opam init -y --disable-sandboxing --bare
opam switch create tryhept 4.14.2 -y || opam switch set tryhept
eval "$(opam env --switch=tryhept)"

# graphics is required so js_of_ocaml-lwt builds its .graphics sublibrary (tryhept links it)
opam install -y \
  ocamlbuild ocamlfind camlp4 menhir menhirLib ocamlgraph graphics \
  js_of_ocaml.5.8.2 js_of_ocaml-ppx.5.8.2 js_of_ocaml-tyxml.5.8.2 js_of_ocaml-lwt \
  ezjs_ace.0.1.1 chartjs.0.2.2

git config --global url."https://git.recherche.enac.fr/".insteadOf "ssh://git@git.recherche.enac.fr/"
git submodule update --init --recursive

# README omits it: the heptagon submodule must be BUILT (heptc.byte + lib .epci), not just configured
(cd heptagon && ./configure && make)

make

ls -la tryhept.js && echo "BUILD_OK"
