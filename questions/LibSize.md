trusty)laheadle@localhost:~/ocaml/tourney$ make install; wc -c tournabox.js; make install debug=1; wc -c tournabox.js

ocamlbuild -cflag -annot -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte
Finished, 33 targets (33 cached) in 00:00:00.
js_of_ocaml +weak.js tournabox.byte
wc -c tournabox.js
143019 tournabox.js

ocamlbuild -lflag -g -cflag -annot -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte
Finished, 33 targets (16 cached) in 00:00:04.
js_of_ocaml +weak.js --enable excwrap --debuginfo --pretty --noinline tournabox.byte
wc -c tournabox.js
683163 tournabox.js


0------ more tests

(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml --debuginfo tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
185401 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
143259 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml --noinline tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
149131 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml --noruntime tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
  caml_array_blit
  caml_backtrace_status
  caml_blit_string
  caml_bytes_of_string
  caml_classify_float
  caml_compare
  caml_convert_raw_backtrace_slot
  caml_create_string
  caml_div
  caml_equal
  caml_failwith
  caml_fill_string
  caml_format_float
  caml_format_int
  caml_get_exception_raw_backtrace
  caml_greaterequal
  caml_hash
  caml_hash_univ_param
  caml_int64_float_of_bits
  caml_int64_format
  caml_int_compare
  caml_int_of_string
  caml_is_printable
  caml_js_call
  caml_js_get_console
  caml_js_html_escape
  caml_js_on_ie
  caml_js_pure_expr
  caml_js_to_byte_string
  caml_js_to_string
  caml_js_wrap_callback
  caml_lessequal
  caml_lex_engine
  caml_make_vect
  caml_md5_string
  caml_ml_flush
  caml_ml_open_descriptor_in
  caml_ml_open_descriptor_out
  caml_ml_out_channels_list
  caml_ml_output
  caml_ml_output_char
  caml_ml_string_length
  caml_mod
  caml_mul
  caml_new_lex_engine
  caml_notequal
  caml_obj_block
  caml_obj_is_block
  caml_obj_set_tag
  caml_obj_tag
  caml_obj_truncate
  caml_register_named_value
  caml_set_oo_id
  caml_string_compare
  caml_string_equal
  caml_string_get
  caml_string_notequal
  caml_string_set
  caml_sys_const_big_endian
  caml_sys_const_ostype_cygwin
  caml_sys_const_ostype_unix
  caml_sys_const_ostype_win32
  caml_sys_const_word_size
  caml_sys_exit
  caml_sys_get_argv
  caml_sys_get_config
  caml_sys_getenv
  caml_sys_random_seed
  caml_update_dummy
143478 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml --pretty tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
328789 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -cflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml --pretty tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
330944 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ rm -rf _build; ocamlbuild -cflag -g -lflag -g -use-ocamlfind -pkgs js_of_ocaml.log,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o tournabox.byte; js_of_ocaml --pretty tournabox.byte; wc -c tournabox.js
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tournabox.ml > tournabox.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules choice.ml > choice.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules country_group.ml > country_group.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o choice.cmo choice.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules columns.ml > columns.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.mli > entry.mli.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.mli > ttypes.mli.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmi entry.mli
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmi ttypes.mli
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules countries.ml > countries.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o columns.cmo columns.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o countries.cmo countries.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules jsutil.ml > jsutil.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules performance_group.ml > performance_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules round_group.ml > round_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules seed_group.ml > seed_group.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tlog.ml > tlog.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules tourney.ml > tourney.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tlog.cmo tlog.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules util.ml > util.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o util.cmo util.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o country_group.cmo country_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o jsutil.cmo jsutil.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o performance_group.cmo performance_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o round_group.cmo round_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o seed_group.cmo seed_group.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tourney.cmo tourney.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o tournabox.cmo tournabox.ml
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules entry.ml > entry.ml.depends
ocamlfind ocamldep -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -modules ttypes.ml > ttypes.ml.depends
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o entry.cmo entry.ml
ocamlfind ocamlc -c -g -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log -o ttypes.cmo ttypes.ml
ocamlfind ocamlc -g -linkpkg -syntax camlp4o -package js_of_ocaml.syntax -package js_of_ocaml -package js_of_ocaml.log choice.cmo util.cmo entry.cmo ttypes.cmo columns.cmo countries.cmo country_group.cmo jsutil.cmo performance_group.cmo round_group.cmo seed_group.cmo tlog.cmo tourney.cmo tournabox.cmo -o tournabox.byte
There are some missing primitives
Dummy implementations (raising 'Failure' exception) will be used if they are not available at runtime.
You can prevent the generation of dummy implementations with the commandline option '-disable genprim'
Missing primitives provided by +weak.js:
  caml_weak_blit
  caml_weak_check
  caml_weak_create
  caml_weak_get
  caml_weak_get_copy
  caml_weak_set
Missing primitives:
381862 tournabox.js
(trusty)laheadle@localhost:~/ocaml/tourney$ 