
FLAGS :=  -cflag -annot -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
DEBUG_FLAGS :=  -lflag -g -cflag -annot -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o

JSO_DEBUG_FLAGS := +weak.js --enable excwrap --debuginfo --pretty --noinline

js:
	ocamlbuild $(FLAGS) tournabox.byte
	js_of_ocaml tournabox.byte
	lessc tournabox.less tournabox.css

dev: js
	cp ./tournabox.css ./tournabox.js ./tst.html ~/Downloads/

# make test CASE=scrolling
test:
	ocamlbuild $(DEBUG_FLAGS) tests/test_$(CASE).byte
	js_of_ocaml $(JSO_DEBUG_FLAGS) test_$(CASE).byte
	cp test_$(CASE).js tests/test_$(CASE).html ~/Downloads/


js_maps:
	ocamlbuild $(DEBUG_FLAGS) tournabox.byte
	js_of_ocaml $(JSO_DEBUG_FLAGS) tournabox.byte
	cp ./tournabox.js ./tst.html ~/Downloads/
