
APPDIRS := $(wildcard apps/*)

define PROXY_TARGET
$(1):
  $(foreach appdir,$(APPDIRS),$(MAKE) -C $(appdir) $(1) ;)
endef

all:
	ocamlbuild -libs str -cflag -g -cflag -bin-annot men_usopen_2014.byte
	ocamlbuild -libs str -cflag -g -cflag -bin-annot women_usopen_2014.byte

