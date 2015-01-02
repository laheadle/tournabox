module C = Choice
module T = Tourney

let report_error str = Dom_html.window##alert (Js.string str)

let (>>=) = Lwt.bind


let select_grouped group_spec tourney =
  let make_choices () = ref [] in
  let choices = make_choices () in
  let convert ichoice = T.choice_of_ichoice ichoice tourney in
  let rec add_choice_iter choice lst =
	match lst with [] -> [[choice]]
	| hd :: tl -> 
	  let group_result = group_spec#in_group choice hd in
	  if group_result.Ttypes.quit then lst
	  else
		if group_result.Ttypes.this_group then (choice :: hd) :: tl
		else
		  hd :: (add_choice_iter choice tl)
  in
  let add_choice = function
	| { C.entry_pair = (Some k, _) } as ichoice ->
	  if not (T.is_bye tourney k) then begin
		Tlog.debugf ~section:Tlog.playing "Add %d" k;
		choices :=
		  add_choice_iter
		  (convert ichoice)
		  !choices
	  end
	| _ -> ()
  in
  for i = 0 to Array.length tourney.T.rounds - 1 do
	let round = tourney.T.rounds.(i) in
	for i = 0 to Array.length round - 1 do
	  match round.(i) with
		{ C.entry_pair = (Some k, Some j) } as ichoice ->
		   add_choice ichoice;
		  (* Make sure the reference entry comes first *)
			add_choice { ichoice with C.entry_pair = ( Some j, Some k ) };
	  | { C.entry_pair = (None, Some k) } as ichoice ->
		add_choice { ichoice with C.entry_pair = ( Some k, None ) };
	  | { C.entry_pair = (Some k, None) } as ichoice ->
		add_choice ichoice;
	  | _ -> (); (* skip empties *)
	done
  done;
  List.sort
	group_spec#compare_group
	(List.map (List.sort group_spec#compare_choice) !choices)

let delete_children node =
  let children = node##childNodes in
  for i = 0 to children##length - 1 do
    Js.Opt.iter (node##firstChild) (fun child -> Dom.removeChild node child) ;
  done

let doc = Dom_html.document

type or_filter = (string -> bool) list

let or_filter_matches or_filter str =
  let rec iter = function
	| [] -> false
	| f :: fs -> f str || iter fs
  in
  iter or_filter

let dom_add ~parent child = Dom.appendChild parent child

let add_td row { Ttypes.class_name; content } =
  let td = Dom_html.createTd Jsutil.doc in
  Jsutil.set_classname td class_name;
  
  let add_fragment = function
	| Ttypes.Text txt ->
	  Dom.appendChild td (Jsutil.textNode txt);
	| Ttypes.Elt {Ttypes.tag;class_name;text} ->
	  let elt = match tag with
		  "span" -> Dom_html.createSpan Jsutil.doc
		| _ -> assert false
	  in
	  Dom.appendChild elt (Jsutil.textNode text);
	  elt##className <- (Js.string class_name);
	  Dom.appendChild td elt
  in
  List.iter add_fragment content;
  Dom.appendChild row td;
  td;

type ('header, 'row) group =
  'header * ('row list ref)

let make_group header = (header, ref [])

let add_row_to_group (_, lst) row =
  lst := !lst @ [row]


let process_groups num_rounds groups grouping_spec (filter: or_filter) =
  let results = ref [] in
  let num_groups = List.length groups in
  let do_group groupi group =
	let num_choices = List.length group in
	let { Ttypes.header; should_filter_header } =
	  grouping_spec#header_spec ~num_rounds ~num_groups ~pos:groupi group in
	let group' = (header, ref []) in
	let has_matches = ref false in
	let do_choice i choice =
	  let row =
		grouping_spec#column_extractor num_choices i choice in
	  let matches =
		List.exists
		  (fun { Ttypes.content;
				 should_filter;
				 class_name = _ } -> 
			let content = Ttypes.column_content_string content in
			let header_content = Ttypes.(column_content_string header.content) in
			(should_filter_header && (or_filter_matches filter header_content)) ||
			  (should_filter && or_filter_matches filter content))
		  row in
	  if matches then begin
		has_matches := true;
		add_row_to_group group' row
	  end
	in
	List.iteri do_choice group;
	if !has_matches then begin
	  results := group' :: !results
	end
  in
  List.iteri do_group groups;
  List.rev !results

let render_groups results groups =
  let do_group _ (header, rows) =
	let header_row = Dom_html.createTr doc in
	header_row##className <- Js.string "tournabox-header-row";
	let header_column = add_td header_row header in
		header_column##colSpan <-
		(match !rows with hd :: _ -> List.length hd | _ -> assert false);
	dom_add ~parent:header_row header_column;
	dom_add ~parent:results header_row;
	let i = ref 0 in
	let do_row _ row =
	  let row_elt = Dom_html.createTr doc in
	  let add_class str =
		  row_elt##className <- (Js.string
								   ((Js.to_string row_elt##className)
									^ " tournabox-" ^ str))
	  in
	  add_class "row";
	  List.iter
		(fun column ->
		  ignore(add_td row_elt column))
		row;
	  begin match !i mod 2 with
		0 ->
		  add_class "even"
	  | _ ->
		  add_class "odd"
	  end;
	  incr i;
	  dom_add ~parent:results row_elt;
	in
	List.iteri do_row !rows
  in
  List.iteri do_group groups

type check = Dom_html.inputElement Js.t

type grouping_spec = Entry.slot Ttypes.grouping_spec

type op =
  Key
| No_Op
| EGroup of check * grouping_spec
| Name_Click of Dom_html.element Js.t

(* Cache of group selections *)
module GroupCache = Map.Make(struct
  type t = grouping_spec
  let compare = compare
end)

(* Cached result of select_grouped *)
type raw_group = Entry.slot C.t list list

type ('results, 'root) state = {
  filter: string;
  current_check_box: check;
  current_egroup: op; (* Egroup *)
  cache: raw_group Lwt.t GroupCache.t;
  all_ops: op list;
  check_boxes: check list;
  tourney: T.tourney;
  root: 'root;
  results: 'results;
  filter_box: Dom_html.inputElement Js.t
}

let all_filters =
  let quoted = Regexp.regexp "^\"(.*)\"$" in
  fun filter ->
	match Regexp.search quoted filter 0 with
	  Some (_, matched) -> begin
		match Regexp.matched_group matched 1 with
		  None -> [""]
		| Some str -> [str]
	  end
	| None ->
	  let commas = Regexp.regexp "\\s*,\\s*" in
	  Regexp.split commas filter

let select_and_render state =
  delete_children state.results;
  let positive = state.current_check_box in
  positive##checked <- Js._true;
  List.iter (fun negative -> negative##checked <- Js._false)
	(List.filter (fun check -> check <> positive ) state.check_boxes);
  let filter =
	let all_filter_strs = all_filters state.filter in
	let make_func filter = fun str ->
	  let result = Util.contains
		(String.lowercase str) (String.lowercase filter) in
	  Tlog.debugf ~section:Tlog.filter "%b: '%s' contains '%s'" result str filter;
	  result in
	List.map make_func all_filter_strs
  in
  match state.current_egroup with
	EGroup (check, espec) ->
	  GroupCache.find espec state.cache >>=
		fun (groups) ->
	  Tlog.noticef ~section:Tlog.grouping "Cache found %d groups" (List.length groups);
	  let num_rounds = T.num_rounds state.tourney in
	  let groups' = process_groups num_rounds groups espec filter in
	  Lwt.return (
		render_groups state.results groups';
	  );
  | _ -> failwith "BUG: get_espec"

let literal_filter str = "\"" ^ str ^ "\""

let rec main_loop state =
  let input_threads =
	let key input =
	  Lwt.bind
		(Lwt_js_events.keyup input)
		(fun event -> Lwt.return Key) in
	let clicks = function
	  | EGroup (positive, _) as g ->
		Lwt.bind
		  (Lwt_js_events.click positive)
		  (fun _ -> Lwt.return g)
	  | _ -> failwith "BUG: bad clicks" in
	let name_clicks =
	  (Lwt_js_events.click state.results) >>=
		(fun event -> Lwt.return
		  (try
			 (Name_Click (Js.Opt.get
							(event##target)
							(fun () -> raise Not_found)))
		   with Not_found -> No_Op))
	in
	(* Js.debugger (); *)
	let clicks = (List.map clicks state.all_ops) in
	name_clicks :: (key state.filter_box) :: clicks
  in
  Lwt.pick input_threads >>= (fun new_op ->
	let new_state =
	  match new_op with
		EGroup (check_box, _) ->
		  state.filter_box##value <- (Js.string "" );
          { state with current_check_box = check_box;
			filter = "";
            current_egroup = new_op }
	  | Key ->
		let value = (Js.to_string state.filter_box##value) in
		  Tlog.noticef ~section:Tlog.filter "in box: %s" value;
		{ state with filter = value }
	  | Name_Click target ->
		begin
		  try
			if Js.to_bool (target##classList##contains
							 (Js.string "tournabox-name")) then
			  let literal = literal_filter (Jsutil.text_of target) in
			  state.filter_box##value <- (Js.string literal );
			  state.root##scrollIntoView (Js._true);
			  let y = Jsutil.offset_of state.root in
			  Dom_html.window##scroll (0, y-100);
			  { state with filter = literal }
			else
			  state
		  with
			Jsutil.Not_text -> state
		end
	  | No_Op -> state
	in
	select_and_render new_state >>=
	  fun () -> main_loop new_state)

let chosen_specs groups_requested =
  let all = [ Round_group.o; Performance_group.o; Country_group.o; Seed_group.o  ] in
  let group_exists group_name =
	List.exists (fun spec ->
	  (String.lowercase group_name) = (String.lowercase spec#name))
	  all in
  let invalid =
	List.filter
	  (fun group_name ->
		not (group_exists group_name))
	  groups_requested in
  let () = if List.length invalid > 0 then
	  ignore(report_error
			   (Printf.sprintf "Invalid Group: '%s'"
				  (String.concat ", " invalid)))
  in
	(* ensure proper ordering *)
  let valid =
	List.filter
	  (fun group_name ->
		group_exists group_name)
	  groups_requested in
  List.map
	(fun valid ->
	  List.find
		(fun spec ->
		  (String.lowercase valid) = (String.lowercase spec#name))
		all)
	valid 

let enter_main_loop state =
  state.filter_box##value <- (Js.string state.filter);
  select_and_render state >>=
	fun () -> main_loop state

let show container groups_requested filters_requested tourney =
  let root =  Dom_html.createDiv doc in
  let results = Dom_html.createTable doc in
  results##className <- (Js.string "tournabox-results");
  let menu = Dom_html.createDiv doc in
  let menu_wrapper = Dom_html.createDiv doc in
  let add_menu elt = dom_add ~parent:menu elt in
  let filter_box = Dom_html.createInput doc in
  let filter_span = Dom_html.createSpan doc in
  let add_group_checkbox group_spec =
	let check_span = Dom_html.createSpan doc in
	let check_group = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
	Jsutil.textNode group_spec#name |> (dom_add ~parent:check_span);
	dom_add ~parent:check_span check_group;
	check_span##className <- (Js.string "tournabox-menu-checkspan");
	add_menu check_span;
	(check_group, group_spec)
  in
  let add_brand () =
	let brand_div = Dom_html.createDiv doc in
	brand_div##className <- (Js.string "tournabox-brand");
	let brand_link = Dom_html.createA doc in
	brand_link##setAttribute (Js.string "href",
							  Js.string "http://laheadle.github.io/docs-tournabox/stable");
	dom_add ~parent:brand_link (Jsutil.textNode "Tournabox");
	dom_add ~parent:brand_div brand_link;
	dom_add ~parent:root brand_div
  in
  let add_menu () =
	menu_wrapper##className <- (Js.string "tournabox-menu-wrapper");
	menu##className <- (Js.string "tournabox-menu");
	filter_span##className <- (Js.string "tournabox-filter-span");
	filter_box##className <- (Js.string "tournabox-filter-input");
	dom_add ~parent:filter_span filter_box;
	dom_add ~parent:menu filter_span;
	dom_add ~parent:root menu_wrapper;
	dom_add ~parent:menu_wrapper menu;
  in
  let () =
	dom_add ~parent:filter_span (Jsutil.textNode "Search: ");
	root##className <- (Js.string "tournabox-root");
	dom_add ~parent:container root;
	add_menu ();
	dom_add ~parent:root results;
	add_brand();
  in
  let especs = List.map add_group_checkbox (chosen_specs groups_requested) in
  let cache = ref GroupCache.empty in
  let emake (check, spec) =
	(* Optimization: Go ahead and do the raw group selection now for
	   each group_spec. This is done using parallel threads; Cache the
	   result. *)
	cache := GroupCache.add spec (Lwt_js.yield () >>= (fun () ->
	  (Lwt.return (select_grouped spec tourney)))) !cache;
	EGroup (check, spec) in
  let get_check (check, _) = check in
  let state = {
	filter = filters_requested;
	current_check_box = get_check (Util.hd_exn especs);
	current_egroup = emake (Util.hd_exn especs);
	all_ops = List.map emake especs;
	check_boxes = List.map get_check especs;
	cache = !cache;
	tourney;
	results;
	root;
	filter_box
  } in
  ignore(enter_main_loop state)

(* A container node and the configuration of a tournabox *)
type 'node tourney_shell = {
  entries: string;
  outcomes:  string;
  groups_requested: string list;
  filters_requested: string;
  container: 'node;
}

let play { entries; outcomes; groups_requested; filters_requested; container } =
  let cleanup_strings () =
	Tlog.infof ~section:Tlog.input "outcomes: %s" outcomes;
	let newline = Regexp.regexp "\n|\\(\r\n\\)" in
	let not_only_spaces str = 
	  let all_spaces = Regexp.regexp "^\\s*$" in
	  match Regexp.search all_spaces str 0 with
		None -> true | _ -> false in
	let entries = List.filter not_only_spaces (Regexp.split newline entries) in
	let outcomes = List.filter not_only_spaces (Regexp.split newline outcomes) in
	let entries = List.map Util.strip_spaces entries in
	let outcomes  = List.map Util.strip_spaces outcomes in
	Tlog.noticef ~section:Tlog.input "%d entries %d outcomes" (List.length entries) (List.length outcomes);
	let entry_of_string =
	  let expect_country =
		List.exists
		  (fun str -> str = "By Country")
		  groups_requested in
	  fun str -> Entry.slot_of_string ~expect_country str in
	let entries = List.map entry_of_string entries in
	(entries, outcomes)
  in
  let (entries, outcomes) = cleanup_strings () in
  let tourney = T.init entries in
	(* let now1 = jsnew Js.date_now () in *)
  let current_state = List.fold_left T.won_str tourney outcomes in
	(* let now2 = jsnew Js.date_now () in 
	   Printf.printf "%d secs to win" now2#getMilliseconds; *)
  show container groups_requested filters_requested current_state

let get_all_tourney_shells () =
  let text_of node_id =
	let node = Jsutil.getElementById_exn node_id in
	try
	  Jsutil.text_of node
	with
	  Jsutil.Not_text -> failwith (Printf.sprintf "The element '#%s' must contain only text" node_id)
	| Jsutil.No_children -> ""
  in
  let get_shell container =
	let space_comma_space = Regexp.regexp "\\s*,\\s*" in
	let groups_requested_str =
	  try
		Jsutil.getAttribute_exn container "tournabox-groups"
	  with _ ->
		let all_group_names = "By Round,By Performance, By Country, By Seed" in
		all_group_names
	in
	let groups_requested = Regexp.split space_comma_space groups_requested_str in
	let id = Js.to_string container##id in
	let entries_node_id =
	  try
		Jsutil.getAttribute_exn container "tournabox-entries"
	  with _ ->
		(id ^ "-entries") in
	let outcomes_node_id =
	  try
		Jsutil.getAttribute_exn container "tournabox-outcomes"
	  with _ ->
		(id ^ "-outcomes") in
	let entries = text_of entries_node_id in
	let outcomes = text_of outcomes_node_id in
	let filters_requested =
	  try
		Jsutil.getAttribute_exn container "tournabox-filters"
	  with _ ->
		""
	in
	{ entries; outcomes; groups_requested; filters_requested; container } in
  let get_shell container =
	try
	  Some (get_shell container)
	with (Failure str) -> report_error str; None in
  let containers = doc##body##querySelectorAll (Js.string ".tournabox-container") in
  let containers = Dom.list_of_nodeList containers in
  let shells = List.map get_shell containers in
  Tlog.noticef ~section:Tlog.input "%d tournament shells found" (List.length shells);
  shells
;;

let unwrap_option = function Some x -> x | None -> assert false in
let somes = function None -> false | Some _ -> true in
let run _ =
  Tlog.infof ~section:Tlog.input "Onload Event Invoked";
  let all = Util.filter_then_map
      ~mapf:unwrap_option ~filterf:somes
      (get_all_tourney_shells ()) in
  List.iter
    (fun shell ->
	   try
	     play shell
	   with Failure str -> report_error str)
    (*	| exn ->  ignore(Lwt_log_js.log ~exn ~level:Lwt_log_js.Error "error"); flush_all ()) 
	  Printf.printf "%s" (Printexc.get_backtrace ()); flush_all ())*)
    all;
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

