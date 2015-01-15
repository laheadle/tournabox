
module Lib = Tournabox_lib
open Ojasmine
open Js
;;

let report_error str = Dom_html.window##alert (Js.string str);;
(* let report_error str = Tlog.errorf "error: %s\n" str; *)

describe (string "a suite exercising tournabox")
  (fun () ->
	let success_parent = Jsutil.getElementById_exn "test-data-successes" in
	let succeeding_containers =
	  List.filter
		(fun elt -> Jsutil.get_classname elt = "tournabox-container")
		(Jsutil.child_elements success_parent) in
	let play_successfully container =
	  match Lib.get_tourney_shell container with
		Some shell, _ ->
		  Lib.play shell;
		  assert_bool true true;
	  | None, str -> failwith str
	in
	let make_success container =
	  it ("plays a container: " ^ 
			 (to_string container##outerHTML))
		(fun () ->
		  play_successfully container)
	in
	List.iter make_success succeeding_containers;
	let expected_error container =
	  Jsutil.getAttribute_exn container "expected-error" in
	let play_unsuccessfully container expected_error =
	  let assert_error str =
		if Util.contains
		  ~within:(String.lowercase str)
		  (String.lowercase expected_error) then
		  assert_bool true true
		else
		  (* Generate failure message *)
		  assert_string str expected_error
	  in
	  match Lib.get_tourney_shell container with
		Some shell, _ ->
		  begin
			try
			  Lib.play shell;
			  assert_bool true false;
			with
			  Failure str ->
				assert_error str
		  end;
	  | None, str ->
		assert_error str
	in
	let make_failure container =
	  let expected_error = expected_error container in
	  it ("fails with error: " ^ expected_error)
		(fun () ->
		  play_unsuccessfully container expected_error)
	in
	let failure_parent = Jsutil.getElementById_exn "failing-containers" in
	let failing_containers = Jsutil.child_elements failure_parent in
	List.iter make_failure failing_containers;
	())
