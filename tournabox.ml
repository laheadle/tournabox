
module Lib = Tournabox_lib

let report_error str = Dom_html.window##alert (Js.string str);;

let run _ =
  Tlog.infof ~section:Tlog.input "Onload Event Invoked";
  let all_good_shells shells =
	List.iter (function (None, str) -> report_error str | _ -> ()) shells;
	let unwrap_option = function (Some x, _) -> x | (None, _) -> assert false in
	let somes = function (None, _) -> false | (Some _, _) -> true in
	Util.filter_then_map ~mapf:unwrap_option ~filterf:somes shells
  in
  let all_good = all_good_shells
	(Lib.get_all_tourney_shells ()) in
  List.iter Lib.play all_good;
  Js._true; (* Allow default action *)
in

Tlog.infof ~section:Tlog.input "Starting Up. This should only happen once.";

ignore(
  Dom_html.addEventListener
    (Dom_html.window)
    Dom_html.Event.load
    (Dom_html.handler run)
    Js._true
)

