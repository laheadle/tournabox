
FLAGS :=  -cflag -annot -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
DEBUG_FLAGS :=  -lflag -g -cflag -annot -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o

JSO_DEBUG_FLAGS := +weak.js --enable excwrap --debuginfo --pretty --noinline

js:
	ocamlbuild $(FLAGS) tourney.byte
	js_of_ocaml tourney.byte

dev: js
	cp ./tourney.js ./tst.html ~/Downloads/

test: js
	ocamlbuild $(DEBUG_FLAGS) test_dom_methods.byte
	js_of_ocaml $(JSO_DEBUG_FLAGS) test_dom_methods.byte
	cp ./test_dom_methods.js ./tst_dom_methods.html ~/Downloads/


js_maps:
	ocamlbuild $(DEBUG_FLAGS) tourney.byte
	js_of_ocaml $(JSO_DEBUG_FLAGS) tourney.byte
	cp ./tourney.js ./tst.html ~/Downloads/
