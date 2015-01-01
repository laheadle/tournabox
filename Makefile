
ifeq ($(debug),)
FLAGS :=  -cflag -annot -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
JSO_FLAGS := +weak.js
else
FLAGS :=  -lflag -g -cflag -annot -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
JSO_FLAGS := +weak.js --enable excwrap --debuginfo --pretty --noinline
endif


js:
	ocamlbuild $(FLAGS) tournabox.byte
	js_of_ocaml $(JSO_FLAGS) tournabox.byte
	lessc tournabox.less tournabox.css

dev: checkTestDir install
	cp `ocamlfind query tournabox`/tournabox.css $(TOURNABOX_TESTDIR)
	cp `ocamlfind query tournabox`/tournabox.js $(TOURNABOX_TESTDIR)
	cp ./tst.html $(TOURNABOX_TESTDIR)

install: uninstall js
	ocamlfind install tournabox META tournabox.js tournabox.css

uninstall:
	ocamlfind remove tournabox

checkTestDir:
ifeq ($(TOURNABOX_TESTDIR),)
	echo "please set the env variable TOURNABOX_TESTDIR to the directory into which your test page should be copied"
	exit 1
endif

# make test CASE=scrolling
test:
	ocamlbuild $(DEBUG_FLAGS) tests/test_$(CASE).byte
	js_of_ocaml $(JSO_DEBUG_FLAGS) test_$(CASE).byte
	cp test_$(CASE).js tests/test_$(CASE).html ~/Downloads/
