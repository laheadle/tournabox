APPDIRS := $(wildcard apps/*)

define PROXY_TARGET
$(1):
  $(foreach appdir,$(APPDIRS),$(MAKE) -C $(appdir) $(1) ;)
endef


js:
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o women_usopen_2014.byte
	js_of_ocaml women_usopen_2014.byte
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o men_usopen_2014.byte
	js_of_ocaml men_usopen_2014.byte

all:
	ocamlbuild -libs str -cflag -g -cflag -bin-annot men_usopen_2014.byte
	ocamlbuild -libs str -cflag -g -cflag -bin-annot women_usopen_2014.byte

