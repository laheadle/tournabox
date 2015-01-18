
project=tournabox

ifeq ($(debug),)
FLAGS :=  -cflag -annot -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
JSO_FLAGS := +weak.js
else
FLAGS :=  -lflag -g -cflag -annot -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
JSO_FLAGS := +weak.js --enable excwrap --debuginfo --pretty --noinline
endif

TESTING_FLAGS := -pkgs ojasmine
templates=tests/.stog/templates
testOutDir=$(TOURNABOX_TESTDIR)/tests

js:
	ocamlbuild $(FLAGS) $(project).byte
	js_of_ocaml $(JSO_FLAGS) $(project).byte
	lessc $(project).less $(project).css

dev: checkTestDir install
	cp `ocamlfind query $(project)`/$(project).css $(TOURNABOX_TESTDIR)
	cp `ocamlfind query $(project)`/$(project).js $(TOURNABOX_TESTDIR)
	cp ./tst.html $(TOURNABOX_TESTDIR)

install: uninstall js
	ocamlfind install $(project) META $(project).js $(project).css $(project).less

uninstall:
	ocamlfind remove $(project)

testSuite: checkTestDir
	ocamlbuild $(FLAGS) $(TESTING_FLAGS) test_tournabox.byte
	js_of_ocaml $(JSO_FLAGS) test_tournabox.byte
	stog tests -d $(testOutDir) --tmpl $(templates)
	cp test_tournabox.js $(testOutDir)

checkTestDir:
ifeq ($(TOURNABOX_TESTDIR),)
	echo "please set the env variable TOURNABOX_TESTDIR to the directory into which your test page should be copied"
	exit 1
else
	rm -rf $(TOURNABOX_TESTDIR)/*
endif

# make question CASE=scrolling
question: dev checkTestDir
	ocamlbuild $(DEBUG_FLAGS) test_$(CASE).byte
	js_of_ocaml $(JSO_DEBUG_FLAGS) test_$(CASE).byte
	cp test_$(CASE).js questions/test_$(CASE).html $(TOURNABOX_TESTDIR)

.PHONY: testSuite js dev install uninstall checkTestDir question
