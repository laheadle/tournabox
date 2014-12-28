module C = Choice

let report_error str = Dom_html.window##alert (Js.string str)

type round =  int Choice.t array
type tourney = { rounds: round array;
				 num_slots: int;
				 byes: (int, bool) Hashtbl.t;
				 entries_list: Entry.t list;
				 entries: (int, Entry.t) Hashtbl.t;
				 indices: (Entry.t, int) Hashtbl.t
			   }

let (>>=) = Lwt.bind

let index_of_entry entry tourney =
  Hashtbl.find tourney.indices entry

let entry_of_index index tourney =
  Hashtbl.find tourney.entries index

let choice_of_ichoice ichoice tourney =
  C.map (fun i ->
	try
	  ignore(Hashtbl.find tourney.byes i);
	  Entry.Bye
	with _ ->
	  Entry.Somebody (entry_of_index i tourney)) ichoice

let log_2 len =
  if Util.power_of_two len then Util.log 2 len
  else failwith ("The number of entries must be a power of two")
	
let num_rounds tourney =
  let len = Array.length tourney.rounds.(0) in
  log_2 len + 1

let entries tourney = tourney.entries
let entries_list tourney = tourney.entries_list

let next_position curr = curr / 2;;

let path entry tourney =
  let rec iter n np lst =
	if n = 0 then lst
	else
	  let nnp = (next_position np) in
	  iter (n - 1) nnp (nnp :: lst)
  in
  List.rev (iter (Array.length tourney.rounds) entry [])

let path_intersect winpath losepath =
  let rec iter i lst1 lst2 =
	match lst1, lst2 with
	  [],_ 	| _,[] -> failwith ("BUG: no intersection of paths")
	| p1::tail1, p2::tail2 ->
	  if (p1 = p2) then
		  (* (playedRound, winI) *)
		i, p1
	  else
		(iter (i+1) tail1 tail2)
  in iter 0 winpath losepath 

let index_to_string tourney index =
  let open Entry in
  to_string (entry_of_index index tourney)

let playing tourney index =
  let path = path index tourney in
  let nth array n = array.(n) in
  let choices = List.map2 nth (Array.to_list tourney.rounds) path in
  let rec find lst = match lst with
	| [] -> failwith ("Error: Invalid Winner:" ^ (index_to_string tourney index) ^
						 ". \n\nThis player has already lost.")
	| { C.entry_pair = (Some x, Some y); winner = None } :: tl
	  -> if x = index then y else x
	| { C.entry_pair = (Some x, None); _ } :: _
	| { C.entry_pair = (None, Some x); _ } :: _ when x = index
	  -> failwith ("Error: Invalid Winner:" ^ (index_to_string tourney index) ^
						 ". \n\nThis player's next opponent is not yet determined.")
	| hd :: tl -> find tl
  in
  find choices

let is_true hash key =
  try
	Hashtbl.find hash key
  with
	_ -> false

let is_bye tourney i = is_true tourney.byes i

let rec won tourney index =
  let impl winner loser = 
	let winpath = path winner tourney in
	let losepath = path loser tourney in
	let (playedRound, winI) = path_intersect winpath losepath in
	let nextI = next_position winI in
	Util.replace (tourney.rounds.(playedRound)) winI (fun playedChoice ->
	  { playedChoice with C.winner = Some winner });
	let schedule () =
	  let next_round = tourney.rounds.(playedRound + 1) in
	  let next_choice = next_round.(nextI) in
	  let will_have_bye, to_play =
		match next_choice with
		  { C.entry_pair = (p1, _p2) ; _ } ->
			assert (_p2 = None);
			match p1 with
			  None ->
				false,
				{ next_choice with C.position = nextI;
				  C.entry_pair = (Some winner, None) }
			  | Some i_p1 ->
				is_bye tourney i_p1,
				{ next_choice with C.position = nextI;
				  C.entry_pair = (p1, Some winner) }
	  in
	  next_round.(nextI) <- to_play;
	  (* Go ahead and advance the bye *)
	  if will_have_bye then won tourney winner else tourney
	in
	if playedRound < num_rounds tourney - 1 then
	  schedule ()
	else
	  tourney
  in
  Tlog.debugf ~section:Tlog.playing "won %d %d\n" index (playing tourney index);
  impl index (playing tourney index)


(* The first round is initialized with full ichoices; other rounds are
   initialized with empty ichoices *)
let init entries_list =
  let open Entry in
  let len = List.length entries_list in
  let num_rounds = log_2 len in
  Tlog.noticef ~section:Tlog.input "%d rounds\n" num_rounds;
  let empty_ichoice ~(round: int) = {
	C.entry_pair=(None,None);
	winner=None;
	round;
	position = 0;
  } in
  let init_round i =
	Array.make (Util.pow 2 (num_rounds - i - 1))
	  (empty_ichoice ~round:i) in
  let byes = Hashtbl.create 4 in
  let init_first round =
	for i = 0 to Array.length round - 1 do
	  let p1 =  i * 2 in
	  let p2 =  i * 2 + 1 in
	  Util.replace round i (fun ichoice ->
		{ ichoice with C.entry_pair=(Some p1, Some p2);
		  position = p1 })
	done
  in
  let entries = Hashtbl.create 200 in
  let indices = Hashtbl.create 200 in
  let rec fill_hashes n lst =
	match lst with
	  [] -> ()
	| hd :: tl ->
	  if Entry.is_bye hd then
		Hashtbl.add byes n true
	  else begin
		let t = fetch hd in
		Hashtbl.add entries n t;
		Hashtbl.add indices t n;
	  end;
	  fill_hashes (n + 1) tl
  in
  fill_hashes 0 entries_list;
  let rounds = Array.init num_rounds init_round in
  let first =  rounds.(0) in
  init_first first;
  let rec perform_byes tourney round_index choice_index =
	Tlog.debugf ~section:Tlog.playing "byes round %d choice %d" round_index choice_index;
	let do_round round =
	  if choice_index < Array.length round then
		let tourney =
		  match round.(choice_index) with
			{ C.entry_pair = Some a, Some b; winner=None } ->
			  if is_true byes a then
				(won tourney b)
			  else if is_true byes b then
				(won tourney a)
			  else
				tourney
		  | _ -> tourney
		in
		perform_byes tourney round_index (choice_index + 1)
	  else
		perform_byes tourney (round_index + 1) 0
	in
	if round_index < Array.length tourney.rounds then
	  do_round rounds.(round_index)
	else
	  tourney
  in
  let tourney = {
	entries_list = Util.filter_then_map ~mapf: fetch
	  ~filterf: is_t
	  entries_list;
	byes;
	entries;
	indices;
	rounds;
	num_slots = Array.length first;
  } in
  perform_byes tourney 0 0

let won_str =
  let hash = Hashtbl.create 200 in
  fun tourney partial ->
	let entry = (
	  try
		Hashtbl.find hash partial
	  with Not_found ->
		let entry =
		  Util.pick (entries_list tourney) partial Entry.to_string
		in
		Hashtbl.add hash partial entry;
		entry) in
	let i = (index_of_entry entry tourney) in
	won tourney i

let num_slots tourney = tourney.num_slots

let select_grouped group_spec tourney =
  let make_choices () = ref [] in
  let choices = make_choices () in
  let convert ichoice = choice_of_ichoice ichoice tourney in
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
	  if not (is_bye tourney k) then begin
		Tlog.debugf ~section:Tlog.playing "Add %d" k;
		choices :=
		  add_choice_iter
		  (convert ichoice)
		  !choices
	  end
	| _ -> ()
  in
  for i = 0 to Array.length tourney.rounds - 1 do
	let round = tourney.rounds.(i) in
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

module GroupCache = Map.Make(struct
  type t = grouping_spec
  let compare = compare
end)

type raw_group = Entry.slot C.t list list

type ('results, 'root) state = {
  filter: string;
  current_check_box: check;
  current_egroup: op; (* Egroup *)
  cache: raw_group Lwt.t GroupCache.t;
  all_ops: op list;
  check_boxes: check list;
  tourney: tourney;
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
	  let num_rounds = num_rounds state.tourney in
	  let groups' = process_groups num_rounds groups espec filter in
	  Lwt.return (render_groups state.results groups');
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
			   (let msg =
				  List.fold_left
					(fun str1 str2 -> Printf.sprintf "%s %s" str1 str2)
					""
					invalid in
				"Invalid Group: '"^msg^"'"))
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
  let add_group_checkbox group_spec =
	let check_span = Dom_html.createSpan doc in
	let check_group = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
	Jsutil.textNode group_spec#name |> (dom_add ~parent:check_span);
	dom_add ~parent:check_span check_group;
	check_span##className <- (Js.string "tournabox-menu-checkspan");
	add_menu check_span;
	(check_group, group_spec)
  in
  let filter_span = Dom_html.createSpan doc in
  let _ =
	root##className <- (Js.string "tournabox-root");
	dom_add ~parent:container root;
	menu_wrapper##className <- (Js.string "tournabox-menu-wrapper");
	menu##className <- (Js.string "tournabox-menu");
	dom_add ~parent:root menu_wrapper;
	dom_add ~parent:menu_wrapper menu;
	dom_add ~parent:root results;
	dom_add ~parent:filter_span (Jsutil.textNode "Filter: ");
	filter_span##className <- (Js.string "tournabox-filter-span");
	filter_box##className <- (Js.string "tournabox-filter-input");
	dom_add ~parent:filter_span filter_box;
	dom_add ~parent:menu filter_span; in
  let especs = List.map add_group_checkbox (chosen_specs groups_requested) in
  let cache = ref GroupCache.empty in
  let emake (check, spec) = 
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

type 'node tourney_spec = {
  entries: string;
  outcomes:  string;
  groups_requested: string list;
  filters_requested: string;
  container: 'node;
}

let play { entries; outcomes; groups_requested; filters_requested; container } =
  let cleanup_strings () =
	Tlog.infof ~section:Tlog.input "%s" outcomes;
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
  let tourney = init entries in
	(* let now1 = jsnew Js.date_now () in *)
  let current_state = List.fold_left won_str tourney outcomes in
	(* let now2 = jsnew Js.date_now () in 
	   Printf.printf "%d secs to win" now2#getMilliseconds; *)
  show container groups_requested filters_requested current_state

let get_all_tourney_specs () =
  let text_of node_id =
	let node = Jsutil.getElementById_exn node_id in
	try
	  Jsutil.text_of node
	with
	  Jsutil.Not_text -> failwith (Printf.sprintf "The element '#%s' must contain only text" node_id)
	| Jsutil.No_children -> ""
  in
  let get_spec container =
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
  let get_spec container =
	try
	  Some (get_spec container)
	with (Failure str) -> report_error str; None in
  let containers = doc##body##querySelectorAll (Js.string ".tournabox-container") in
  let lst = Dom.list_of_nodeList containers in
  let specs = List.map get_spec lst in
  Tlog.noticef ~section:Tlog.input "%d tournament specs found" (List.length specs);
  specs
;;

let unwrap_option = function Some x -> x | None -> assert false in
let somes = function None -> false | Some _ -> true in
let all = Util.filter_then_map
  ~mapf:unwrap_option ~filterf:somes
  (get_all_tourney_specs ()) in
List.iter
  (fun spec ->
	try
	  play spec
	with Failure str -> report_error str)
(*	| exn ->  ignore(Lwt_log_js.log ~exn ~level:Lwt_log_js.Error "error"); flush_all ()) 
	  Printf.printf "%s" (Printexc.get_backtrace ()); flush_all ())*)
  all
