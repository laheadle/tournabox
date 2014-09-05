
all:
	ocamlbuild -libs str -cflag -g -cflag -bin-annot men_usopen_2014.byte
	ocamlbuild -libs str -cflag -bin-annot women_usopen_2014.byte

