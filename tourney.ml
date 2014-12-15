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
  Printf.printf "%d rounds" (log_2 len + 1);
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
  Printf.printf "won %d %d" index (playing tourney index); flush_all();
  impl index (playing tourney index)


(* The first round is initialized with full ichoices; other rounds are
   initialized with empty ichoices *)
let init entries_list =
  let open Entry in
  let len = List.length entries_list in
  Printf.printf "#entries: %d\n" len;
  let num_rounds = log_2 len in
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
	Printf.printf "byes %d %d" round_index choice_index; flush_all ();
	let do_round round =
	  if choice_index < Array.length round then
		let tourney =
		  match round.(choice_index) with
			{ C.entry_pair = Some a, Some b } ->
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
		(Printf.printf "Add %d" k); flush_all();
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

let render_groups tourney container groups grouping_spec (filter: or_filter) =
  let num_rounds = num_rounds tourney in
  let num_groups = List.length groups in
  let do_choices groupi choices =
	let num_choices = List.length choices in
	let { Ttypes.header_str; should_filter_header } =
	  grouping_spec#header_spec ~num_rounds ~num_groups ~pos:groupi choices in
	let table = Jsutil.table (Some "tourney-outerTable") in
	let header = Dom_html.createTr doc in
	header##className <- Js.string "tourney-header-row";
	Jsutil.addTd header header_str (Some "tourney-header");
	Dom.appendChild table header;
	let has_matches = ref false in
	let do_choice i choice =
	  let row = Dom_html.createTr doc in
	  let columns =
		grouping_spec#column_extractor num_choices i choice in
	  let matches =
		List.exists
		  (fun { Ttypes.content;
				 should_filter;
				 class_name = _ } -> 
			(should_filter_header && (or_filter_matches filter header_str)) ||
			  (should_filter && or_filter_matches filter content))
		  columns in
	  if matches then begin
		has_matches := true;
		List.iter
		  (fun { Ttypes.class_name; content } ->
			Jsutil.addTd row content class_name)
		  columns;
		Dom.appendChild table row
	  end
	in
	List.iteri do_choice choices;
	if !has_matches then begin
	  Dom.appendChild container table
	end
  in
  List.iteri do_choices groups

type check = Dom_html.inputElement Js.t

type op =
  Key
| EGroup of check * Entry.slot Ttypes.grouping_spec

type 'node state = {
  filter: string;
  current_check_box: check;
  current_egroup: op; (* Egroup *)
  all_ops: op list;
  check_boxes: check list;
  tourney: tourney;
  inner: 'node;
  filter_box: Dom_html.inputElement Js.t
}

let select_and_render state =
  delete_children state.inner;
  let positive = state.current_check_box in
  positive##checked <- Js._true;
  List.iter (fun negative -> negative##checked <- Js._false)
	(List.filter (fun check -> check <> positive ) state.check_boxes);
  let filter =
	let commas = Regexp.regexp "\\s*,\\s*" in
	let all_filter_strs = Regexp.split commas state.filter in
	let make_func filter = fun str ->
	  let result = Util.contains
		(String.lowercase str) (String.lowercase filter) in
		(*Printf.printf "%b: %s" result str; flush_all (); *)
	  result in
	List.map make_func all_filter_strs
  in
  match state.current_egroup with
	EGroup (check, espec) ->
	  let groups = select_grouped espec state.tourney in
	  Printf.printf "Selected %d groups" (List.length groups); flush_all();
	  render_groups state.tourney state.inner groups espec filter
  | _ -> failwith "BUG: get_espec"

let rec main_loop state =
  let key input =
	Lwt.bind
	  (Lwt_js_events.keyup input)
	  (fun event -> Lwt.return Key) in
  let clicks = function
	| EGroup (positive, _) as g ->
	  Lwt.bind
		(Lwt_js_events.click positive)
		(fun _ -> Lwt.return g)
	| _ -> failwith "BUG: bad clicks"
  in
	(* Js.debugger (); *)
  let threads =
	let clicks = (List.map clicks state.all_ops) in
	(key state.filter_box) :: clicks in
  Lwt.pick threads >>= (fun new_op ->
	let new_state =
	  match new_op with
		EGroup (check_box, _) ->
          { state with current_check_box = check_box;
            current_egroup = new_op }
	  | Key ->
		let value = (Js.to_string state.filter_box##value) in
		  (* Printf.printf "%s" value; flush_all (); *)
		{ state with filter = value } in
	select_and_render new_state;
	ignore (main_loop new_state);
	Lwt.return ())

let chosen_specs groups_requested =
  let all = [ Round_group.o; Performance_group.o; Country_group.o; Seed_group.o ] in
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
  select_and_render state;
  main_loop state

let show container groups_requested filters_requested tourney =
  let inner = Dom_html.createDiv doc in
  let top = Dom_html.createDiv doc in
  let domAdd = Dom.appendChild in
  let add elt = domAdd container elt in
  let addTop elt = domAdd top elt in
  let middle = Dom_html.createDiv doc in
  let filter_box = Dom_html.createInput doc in
  let add_group_checkbox group_spec =
	let check_span = Dom_html.createSpan doc in
	let check_group = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
	Jsutil.textNode group_spec#name |> (domAdd check_span);
	domAdd check_span check_group;
	check_span##className <- (Js.string "tourney-menu-checkspan");
	addTop check_span;
	(check_group, group_spec)
  in
  let filter_span = Dom_html.createSpan doc in
  let _ =
	top##className <- (Js.string "tourney-menu");
	add top;
	add middle;
	domAdd container middle;
	add inner;
	domAdd filter_span (Jsutil.textNode "Filter: ");
	filter_span##className <- (Js.string "tourney-filterBox");
	domAdd filter_span filter_box;
	domAdd top filter_span; in
  let add = add_group_checkbox in
  let especs = List.map add (chosen_specs groups_requested) in
  let emake (check, spec) = EGroup (check, spec) in
  let get_check (check, _) = check in
  let state = {
	filter = filters_requested;
	current_check_box = get_check (Util.hd_exn especs);
	current_egroup = emake (Util.hd_exn especs);
	all_ops = List.map emake especs;
	check_boxes = List.map get_check especs;
	tourney;
	inner;
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
  (* Printf.printf "%s" outcomes; flush_all (); *)
	let newline = Regexp.regexp "\n|\\(\r\n\\)" in
	let not_only_spaces str = 
	  let all_spaces = Regexp.regexp "^\\s*$" in
	  match Regexp.search all_spaces str 0 with
		None -> true | _ -> false in
	let entries = List.filter not_only_spaces (Regexp.split newline entries) in
	let outcomes = List.filter not_only_spaces (Regexp.split newline outcomes) in
	let entries = List.map Util.strip_spaces entries in
	let outcomes  = List.map Util.strip_spaces outcomes in
  (* Printf.printf "%d en %d ou" (List.length entries) (List.length outcomes) ; flush_all(); *)
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
  Printf.printf "showing"; flush_all ();
  show container groups_requested filters_requested current_state

exception Not_text

let get_all_tourney_specs () =
  let first_child node =
	Js.Opt.get (node##childNodes##item(0)) (fun () -> raise Not_found) in
  let text_child node =
	let child = first_child node in
	let opt = Dom.CoerceTo.text child in
	Js.Opt.get opt (fun () -> raise Not_text)  in
  let text_of node_id =
	let node = Jsutil.getElementById_exn node_id in
	try
	  Js.to_string (text_child node)##data
	with
	  Not_text -> failwith (Printf.sprintf "The element '#%s' must contain only text" node_id)
	| Not_found -> ""
  in
  let get_spec container =
	let space_comma_space = Regexp.regexp "\\s*,\\s*" in
	let groups_requested_str =
	  try
		Jsutil.getAttribute_exn container "tourney-groups"
	  with _ ->
		let all_group_names = "By Round,By Performance, By Country, By Seed" in
		all_group_names
	in
	let groups_requested = Regexp.split space_comma_space groups_requested_str in
	let id = Js.to_string container##id in
	let entries_node_id =
	  try
		Jsutil.getAttribute_exn container "tourney-entries"
	  with _ ->
		(id ^ "-entries") in
	let outcomes_node_id =
	  try
		Jsutil.getAttribute_exn container "tourney-outcomes"
	  with _ ->
		(id ^ "-outcomes") in
	let entries = text_of entries_node_id in
	let outcomes = text_of outcomes_node_id in
	let filters_requested =
	  try
		Jsutil.getAttribute_exn container "tourney-filters"
	  with _ ->
		""
	in
	{ entries; outcomes; groups_requested; filters_requested; container } in
  let get_spec container =
	try
	  Some (get_spec container)
	with (Failure str) -> report_error str; None in
  let containers = doc##body##querySelectorAll (Js.string ".tourney-container") in
  let lst = Dom.list_of_nodeList containers in
  let specs = List.map get_spec lst in
	(* Printf.printf "%d found" (List.length mapped); flush_all(); *)
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
