bold=$(shell tput bold)
normal=$(shell tput sgr0)

SRC_DIR := heptc/src
EXTRACTED := heptc/extraction/extracted

FLAGS=-use-ocamlfind -Is heptagon/compiler/ \
      -pkgs str,unix,menhirLib,ocamlgraph,js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-tyxml,js_of_ocaml-lwt.graphics,ezjs_ace,chartjs \
	  -no-hygiene

SRC := \
	examples.ml \
	hept_scoping2.ml \
	compil.ml \
	chronogram.ml \
	page.ml \
	js_obc_conversion.ml \
	simul.ml interp.ml \
	export.ml \
	tryhept.ml \
	pervasives.ml mathlib.ml

all: tryhept.js

examples.ml:
	ocaml preproc_examples.ml

tryhept.byte: $(SRC)
	@echo "${bold}Building tryhept...${normal}"
	ocamlbuild ${FLAGS} tryhept.byte
	@echo "${bold}Done.${normal}"

tryhept.js: tryhept.byte
	js_of_ocaml $^

%.epci: heptagon/lib/%.epi
	cd heptagon/lib && make
	cp heptagon/lib/$@ .

%.ml: %.epci embed_epci.ml
	ocaml embed_epci.ml $^ > $@

clean:
	rm -rf _build/ *.byte tryhept.js pervasives.epci

.PHONY:
	all clean extraction
