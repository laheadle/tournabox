APPDIRS := $(wildcard apps/*)

define PROXY_TARGET
$(1):
  $(foreach appdir,$(APPDIRS),$(MAKE) -C $(appdir) $(1) ;)
endef

test:
	ocamlbuild -cflag -g -cflag -annot  -lflag -g -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o test_checks.byte
	js_of_ocaml --pretty --debuginfo test_checks.byte
	cp ./test_checks.js ./tst_checks.html ~/Downloads/

js:
	ocamlbuild -cflag -g -cflag -annot  -lflag -g -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o women_usopen_2014.byte
	js_of_ocaml --pretty --debuginfo women_usopen_2014.byte
	cp ./women_usopen_2014.map ./women_usopen_2014.js ./tst.html ~/Downloads/

	ocamlbuild -cflag -g -cflag -annot -lflag -g -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o men_usopen_2014.byte
	js_of_ocaml --pretty --debuginfo --sourcemap men_usopen_2014.byte

all:
	ocamlbuild -libs str -cflag -g -cflag -bin-annot men_usopen_2014.byte
	ocamlbuild -libs str -cflag -g -cflag -bin-annot women_usopen_2014.byte

