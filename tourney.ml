module C = Choice

module type S = sig
  type e

  type round_in_progress = e Choice.t list

  type tourney

  val num_rounds: tourney -> int

  val entries: tourney -> e list
  val num_entries: tourney -> int

  val play: entries:string list -> outcomes:string list -> unit
end

module Make(League: League.S) = struct

  module Entry = Entry.Make(League.Player)

  type e = Entry.t

  type round =  int Choice.t array
  type tourney = { rounds: round array; entries: Entry.t list }
  type round_in_progress = e Choice.t list

  let index_of_entry entry tourney =
	let rec find i lst =
	  match lst with
		[] -> raise (Invalid_argument "entry not found")
	  | hd :: tl -> 
		if hd = entry then i
		else find (i + 1) tl
	in
	find 0 tourney.entries

  let entry_of_index index tourney =
	let rec find i lst =
	  match lst with
		[] -> raise (Invalid_argument "entry not found")
	  | hd :: tl -> 
		if i = index then hd
		else find (i + 1) tl
	in
	find 0 tourney.entries

  let choice_of_ichoice ichoice tourney =
	C.map (fun i -> entry_of_index i tourney) ichoice

  let num_rounds tourney =
	Array.length tourney.rounds

  let init entries =
	let len = List.length entries in
	Printf.printf "#entries: %d\n" len;
	let num_rounds = if Util.power_of_two len then Util.log 2 len
	  else raise (Invalid_argument "entry length not a power of 2") in
	let empty_ichoice ~(round: int) = {
	  C.entry_pair=(None,None);
	  winner=None;
	  round = round;
	  position = 0;
	} in
	let init_round i =
	  Array.make (Util.pow 2 (num_rounds - i - 1))
		(empty_ichoice ~round:i) in
	let init_first tourney = 
	  let round = tourney.rounds.(0) in
	  for i = 0 to Array.length round - 1 do
		let p1 =  i * 2 in
		let p2 =  i * 2 + 1 in
		Util.replace round i (fun ichoice ->
		  { ichoice with C.entry_pair=(Some p1, Some p2);
			position = p1 })
	  done
	in
	let tourney = { 
	  entries = entries;
	  rounds = Array.init num_rounds init_round } in
	init_first tourney;
	tourney

  let entries tourney = tourney.entries

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
		[],_ 	| _,[] -> raise (Invalid_argument "bad lengths")
	  | p1::tail1, p2::tail2 ->
		if (p1 = p2) then
		  (* (playedRound, winI) *)
		  i, p1
		else
		  (iter (i+1) tail1 tail2)
	in iter 0 winpath losepath 


  let playing tourney entry =
	let path = path entry tourney in
	let nth array n = List.nth (Array.to_list array) n in
	let choices = List.map2 nth (Array.to_list tourney.rounds) path in
	let rec find lst = match lst with
	  | [] -> raise (Invalid_argument "no games to win")
	  | { C.entry_pair = (Some x, Some y); winner = None } :: tl
		-> if x = entry then y else x
	  | hd :: tl -> find tl
	in
	find choices

  let won tourney entry =
	let impl winner loser = 
	  let winpath = path winner tourney in
	  let losepath = path loser tourney in
	  let (playedRound, winI) = path_intersect winpath losepath in
	  let nextI = next_position winI in
	  Util.replace (tourney.rounds.(playedRound)) winI (fun playedChoice ->
		{ playedChoice with C.winner = Some winner });
	  if playedRound < num_rounds tourney - 1 then
		let schedule nextChoice =
		  let to_play =
			match nextChoice with
			  { C.entry_pair = (p1, _p2) ; _ } ->
				assert (_p2 = None);
				if p1 = None then
				  { nextChoice with C.entry_pair = (Some winner, None) }
				else
				  { nextChoice with C.entry_pair = (p1, Some winner) } in
		  { to_play with C.position = nextI }
		in
		Util.replace (tourney.rounds.(playedRound + 1)) nextI schedule
	  else
		();
	  tourney
	in
	impl (index_of_entry entry tourney)
	  (playing tourney (index_of_entry entry tourney))


  let won_str tourney partial =
	let entry =
	  Util.pick (entries tourney) partial Entry.to_string
	in
	won tourney entry

  let num_entries tourney = List.length tourney.entries

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
	let add_choice ichoice =
	  choices :=
		add_choice_iter
		(group_spec#convert (convert ichoice))
		!choices
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

  let render_groups tourney container groups grouping_spec (filter: string -> bool) =
	let num_rounds = num_rounds tourney in
	let do_choices groupi choices =
	  let num_choices = List.length choices in
	  let header_str =
		grouping_spec#header_name ~num_rounds ~pos:groupi choices in
	  let table = Jsutil.table (Some "tourney-outerTable") in
	  let header = Dom_html.createTr doc in
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
			  if should_filter then 
				(filter header_str || filter content )
			  else
				false)
			columns in
		if matches then begin
			has_matches := true;
			List.iter (fun { Ttypes.class_name; content } -> Jsutil.addTd row content class_name) columns;
			Dom.appendChild table row
		  end
	  in
	  List.iteri do_choice choices;
	  if !has_matches then begin
		Dom.appendChild container table
	  end
	in
	List.iteri do_choices groups

  let contains_player lst player =
	let rec contains_iter = function
	  | [] -> false
	  | hd :: tl ->
		let head_matches =
		  match hd with
			{ C.entry_pair = (Some a, Some b) } ->
			  a = player || b = player
		  |	{ C.entry_pair = (Some a, None) } ->
			a = player
		  | { C.entry_pair = (None, Some b) } ->
			b = player
		  | { C.entry_pair = (None, None) } ->
			false
		in
		if head_matches then true
		else
		  contains_iter tl
	in
	contains_iter lst

  let contains_choice_player lst choice =
	match choice with
	| { C.entry_pair = (Some a, Some b) } ->
	  (contains_player lst a) || (contains_player lst b)
	|	{ C.entry_pair = (Some a, None) } ->
	  contains_player lst a
	| { C.entry_pair = (None, Some b) } ->
	  contains_player lst b
	| { C.entry_pair = (None, None) } ->
	  false

  let round_group =
	(object
	  method name = "By Round"
	  method header_name ~num_rounds ~pos:round lst =
		if round = 0 then
		  "Finals\n"
		else
		  (if round = 1 then
			  "Semifinals"
		   else
			  (if round = 2 then
				  "Quarterfinals"
			   else
				  (Printf.sprintf
					 "Round %d (%d matches)"
					 (num_rounds - round)
					 (List.length lst))))
	  method compare_choice a b = compare a b
	  method compare_group = C.compare_length_then_first
	  method in_group choice group =
		let (round_matches, already) = match group with
			{ C.round = r1; _ } :: _ ->
			  (choice.C.round = r1), (contains_choice_player group choice)
		  | _ -> failwith "Invalid group" in

		{
		  Ttypes.quit = round_matches && already;
		  this_group = round_matches && not already
		}
	  method convert x = x
	  method column_extractor num pos choice =
		let extractors = match choice with
		  | { C.entry_pair = Some a, Some b; winner = Some c } ->
			let loser = if c = a then b else a in
			[ (Entry.to_string c), None, true;
			  "defeated", (Some "tourney-won"), false;
			  (Entry.to_string loser), None, true ]
		  | { C.entry_pair = Some a, Some b; winner = None } ->
			[ (Entry.to_string a), None, true;
			  "will face", (Some "tourney-willFace"), false;
			  (Entry.to_string b), None, true ]
		  | { C.entry_pair = Some a, None; _ } ->
			[ (Entry.to_string a), None, true;
			  "will face", (Some "tourney-willFace"), false;
			  "(To be decided)", None, false ]
		  | _ ->
			failwith "bug 4" in
		List.map Ttypes.make_column_extractor extractors
	 end)

  let performance_group = 
	(object
	  method name = "By Performance"
	  method header_name ~num_rounds ~pos lst =
		C.extract_first_first lst (fun e -> Entry.to_string e)
	  method compare_choice c1 c2 = -(compare c1 c2)
	  method compare_group =  fun g1 g2 -> -(C.compare_length_then_first g1 g2)
	  method in_group choice group = {
		Ttypes.quit = false;
		this_group =
		  (match choice with
			{ C.entry_pair = (Some a), _ ; _ }
			-> (match group with
			  { C.entry_pair = (Some b), _ } :: _ ->
				a = b
			| _ -> failwith "Bad existing member")
		  | _ -> failwith "Bad choice for group")
	  }
	  method convert x = x
	  method column_extractor num pos choice =
		let extractors =
		  match choice with
		  | { C.entry_pair = Some a, Some b; winner = Some c } ->
			let outcome = if c = a then "Defeated" else "Was defeated by" in
			[ outcome,
			  Some (if c = a then "tourney-won" else "tourney-lost"),
			  false;
			  (Entry.to_string b), None, true;
			  ("In round " ^ (string_of_int (num - pos))), None, false ]
		  | { C.entry_pair = Some a, Some b; winner = None } ->
			[ "will play", None, false;
			  (Entry.to_string b), None, true ]
		  | { C.entry_pair = Some a, None; winner = None } ->
			[ "will play", None, false;
			  "To be determined", None, false ]
		  | _ -> failwith "bug" in
		List.map Ttypes.make_column_extractor extractors
	 end)

  type check = Dom_html.inputElement Js.t
  type ('a, 'b) gspec = ('a, 'b) Ttypes.converted_grouping_spec

  type op =
	Key
  | EGroup of check * (Entry.t, Entry.t) gspec
  | PGroup of check * (Entry.player, Entry.t) gspec

(*
  let get_espec: 'a 'b. ('a * 'b) -> ('a * 'a)
	= fun (x, y) -> (x, x)

  let get_espec: 'a 'b. ('a, 'b) op -> ('a, 'a) gspec
	= function
	| Egroup (check, gspec) -> gspec
	| _ -> failwith "get_espec"
*)

  type 'node state = {
	filter: string;
	current_check_box: check;
	current_op: op;
	all_ops: op list;
	check_boxes: check list;
	tourney: tourney;
	inner: 'node
  }

  let show tourney =
	let container = Jsutil.getElementById_exn "container" in
	let inner = Dom_html.createDiv doc in
	let top = Dom_html.createDiv doc in
	let domAdd = Dom.appendChild in
	let add elt = domAdd container elt in
	let addTop elt = domAdd top elt in
	let select_and_render state =
	  delete_children state.inner;
	  let positive = state.current_check_box in
	  positive##checked <- Js._true;
	  List.iter (fun negative -> negative##checked <- Js._false)
		(List.filter (fun check -> check <> positive ) state.check_boxes);
	  let filter_func str =
		let result = Util.contains
		  (String.lowercase str) (String.lowercase state.filter) in
		   (*Printf.printf "%b: %s" result str; flush_all (); *)
		result in
	  match state.current_op with
		EGroup (check, espec) ->
		  let groups = select_grouped espec tourney in
		  render_groups tourney inner groups espec filter_func
	  | PGroup (check, pspec) ->
		let groups = select_grouped pspec tourney in
		render_groups tourney inner groups pspec filter_func
	  | _ -> failwith "get_espec"
	in
	let middle = Dom_html.createDiv doc in
	let filter_box = Dom_html.createInput doc in
	let key input =
	  Lwt.bind
		(Lwt_js_events.keyup input)
		(fun event -> Lwt.return Key) in
	let clicks = function
	  | EGroup (positive, _) as g ->
		Lwt.bind
		  (Lwt_js_events.click positive)
		  (fun _ -> Lwt.return g)
	  | PGroup (positive, _) as g ->
		Lwt.bind
		  (Lwt_js_events.click positive)
		  (fun _ -> Lwt.return g)
	  | _ -> failwith "bad clicks"
	in
	let rec main_loop state =
	  (* Js.debugger (); *)
	  let threads =
		let clicks = (List.map clicks state.all_ops) in
		(key filter_box) :: clicks in
	  let triggered = Lwt.pick threads in
	  Lwt.bind triggered (fun new_op ->
		let new_state =
		  match new_op with
			EGroup (check_box, _) ->
			  { state with current_check_box = check_box;
				current_op = new_op }
		  | PGroup (check_box, _) ->
			{ state with current_check_box = check_box;
			  current_op = new_op }
		  | Key ->
			let value = (Js.to_string filter_box##value) in
			(* Printf.printf "%s" value; flush_all (); *)
			{ state with filter = value } in
		select_and_render new_state;
		ignore (main_loop new_state);
		Lwt.return ())
	in
	let enter_main_loop state =
	  select_and_render state;
	  main_loop state
	in
	let add_group_checkbox group_spec =
	  let checkGroup = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
	  Jsutil.textNode group_spec#name |> addTop;
	  addTop checkGroup;
	  (checkGroup, group_spec)
	in
	let filter_span = Dom_html.createSpan doc in
	let _ =
	  top##className <- (Js.string "tourney-menuDiv");
	  add top;
	  add middle;
	  domAdd container middle;
	  add inner;
	  domAdd filter_span (Jsutil.textNode "Filter: ");
	  filter_span##className <- (Js.string "tourney-filterBox");
	  domAdd filter_span filter_box;
	  domAdd top filter_span; in
	let (check_rounds, by_round) as round = add_group_checkbox round_group in
	let (check_performance, by_performance) as perf =
	  add_group_checkbox (performance_group) in
	let add = add_group_checkbox in
	let pspecs = List.map add Entry.player_specs in
	let especs = List.map add Entry.entry_specs in
	let emake (check, spec) = EGroup (check, spec) in
	let (ops, checks) =
	  let all_especs = round :: perf :: especs in
	  let pmake (check, spec) = PGroup (check, spec) in
	  let get_check (check, _) = check in
	  (List.map emake all_especs @
		 List.map pmake pspecs),
	  (List.map get_check all_especs @
		 List.map get_check pspecs)
	in
	let state = {
	  filter = "";
	  current_check_box = check_rounds;
	  current_op = emake (check_rounds, by_round);
	  all_ops = ops;
	  check_boxes = checks;
	  tourney;
	  inner
	} in

	ignore(enter_main_loop state)




  let play ~entries ~outcomes =
	let db = League.make_db () in
	let make_entry str =
	  let (player_str, data) = Entry.parse str in
	  let player = League.pick player_str db in
	  Entry.make player data
	in	
	let entries = List.map make_entry entries in
	let tourney = init entries in
	let current_state = List.fold_left won_str tourney outcomes in

	(* Tourney.print current_state Tennis_player_entry.to_string *)
	show current_state

end
