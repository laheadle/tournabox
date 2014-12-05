APPDIRS := $(wildcard apps/*)

define PROXY_TARGET
$(1):
  $(foreach appdir,$(APPDIRS),$(MAKE) -C $(appdir) $(1) ;)
endef

test:
	ocamlbuild -cflag -g -cflag -annot  -lflag -g -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o test_dom_methods.byte
	js_of_ocaml --pretty --debuginfo test_dom_methods.byte
	#cp ./test_dom_methods.js ./tst_dom_methods.html ~/Downloads/

js:
	ocamlbuild -cflag -annot -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tourney.byte
	js_of_ocaml tourney.byte
	cp ./tourney.js ./tst.html ~/Downloads/

js_maps:
	ocamlbuild -lflag -g -cflag -annot -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tourney.byte
	js_of_ocaml +weak.js --enable excwrap --debuginfo --pretty --noinline tourney.byte
	cp ./tourney.js ./tst.html ~/Downloads/

all:
	ocamlbuild -libs str -cflag -g -cflag -bin-annot men_usopen_2014.byte
	ocamlbuild -libs str -cflag -g -cflag -bin-annot women_usopen_2014.byte

