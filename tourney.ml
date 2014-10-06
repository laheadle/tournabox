
module type S = sig
  type e

  type choice = {
	entry_pair : e option * e option;
	winner : e option;
  }

  type round_in_progress = choice list

  type tourney

  val num_rounds: tourney -> int

  val entries: tourney -> e list
  val num_entries: tourney -> int

  val play: entries:string list -> outcomes:string list -> unit
end

module Make(League: League.S) = struct

  module Entry = Entry.Make(League.Player)

  type e = Entry.t
  type choice = {
	entry_pair: Entry.t option * Entry.t option;
	winner: Entry.t option
  }
	
  type ichoice = {
	ientry_pair: int option * int option;
	winner: int option
  }

  type round =  ichoice array
  type tourney = { rounds: round array; entries: Entry.t list }
  type round_in_progress = choice list

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

  let choice_of_ichoice { ientry_pair=(p1,p2); winner } tourney =
	let convert opt = match opt with
		None -> None 
	  | Some pi -> Some (entry_of_index pi tourney)
	in
	{ entry_pair = (convert p1, convert p2); winner = convert winner }

  let num_rounds tourney =
	Array.length tourney.rounds

  let init entries =
	let len = List.length entries in
	Printf.printf "#entries: %d\n" len;
	let num_rounds = if Util.power_of_two len then Util.log 2 len
	  else raise (Invalid_argument "entry length not a power of 2") in
	let empty_ichoice: ichoice = { ientry_pair=(None,None); winner=None } in
	let init_round i =
	  Array.make (Util.pow 2 (num_rounds - i - 1)) empty_ichoice in
	let init_first tourney = 
	  let round = tourney.rounds.(0) in
	  for i = 0 to Array.length round - 1 do
		let p1 =  i * 2 in
		let p2 =  i * 2 + 1 in
		Util.replace round i (fun ichoice ->
		  { ichoice with ientry_pair=(Some p1, Some p2) })
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
	  | { ientry_pair = (Some x, Some y); winner = None } :: tl
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
		{ playedChoice with winner = Some winner });
	  if playedRound < num_rounds tourney - 1 then
		let schedule nextChoice = match nextChoice with
			{ ientry_pair = (p1, _p2) ; _ } ->
			  assert (_p2 = None);
			  if p1 = None then
				{ nextChoice with ientry_pair = (Some winner, None) }
			  else
				{ nextChoice with ientry_pair = (p1, Some winner) }
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

  let filter_choices tourney (fn: ichoice -> bool) =
	let filter round =
	  let ichoices = List.filter 
		fn
		(Array.to_list round)
	  in
	  List.map (fun ic ->
		choice_of_ichoice ic tourney)
		ichoices
	in
	List.map filter (Array.to_list tourney.rounds)

  let filter_choices_paired tourney fn1 fn2 =
	let mapper round =
	  let filter fn =
		List.filter 
		  fn
		  (Array.to_list round) in
	  let tochoices ichoices =
		List.map
		  (fun ic -> choice_of_ichoice ic tourney)
		  ichoices
	  in
	  let ichoices1 = filter fn1 in
	  let ichoices2 = filter fn2 in
	  (tochoices ichoices1, tochoices ichoices2)
	in
	List.map mapper (Array.to_list tourney.rounds)

  let nonempty_choices tourney =
	let undecided =
	  function { ientry_pair = None, None ; _ } -> false
	  | { winner = Some _ } -> false
	  | _ -> true
	in
	let decided =
	  function { winner = Some _ } -> true
	  | _ -> false
	in
	filter_choices_paired tourney undecided decided

  let num_entries tourney = List.length tourney.entries

  let choices_per_entry tourney ~compare_entry =
	let choices = Array.make (num_entries tourney) [] in
	let replace k ic =
	  Util.replace choices k (fun lst ->
		(choice_of_ichoice ic tourney)  :: lst) in
	for i = 0 to Array.length tourney.rounds - 1 do
	  let round = tourney.rounds.(i) in
	  for i = 0 to Array.length round - 1 do
		match round.(i) with
		  { ientry_pair = (Some k, Some j) } as ichoice ->
			replace k ichoice;
		  (* Make sure the reference entry comes first *)
			replace j { ichoice with ientry_pair = ( Some j, Some k ) };
		| { ientry_pair = (None, Some k) } as ichoice ->
		  replace k { ichoice with ientry_pair = ( Some k, None ) };
		| { ientry_pair = (Some k, None) } as ichoice ->
		  replace k ichoice;
		| _ -> (); (* skip empties *)
	  done
	done;
	Array.sort compare_entry choices;
	choices

  let delete_children node =
	let children = node##childNodes in
	for i = 0 to children##length - 1 do
      Js.Opt.iter (node##firstChild) (fun child -> Dom.removeChild node child) ;
	done

  let doc = Dom_html.document

  let print_by_entry tourney container =
	let by_entry = choices_per_entry tourney 
	  ~compare_entry: (fun lst1 lst2 ->
		compare (List.length lst2) (List.length lst1)) in

	let do_entry_choices i choices =
	  let entry_str =
 		(match List.hd choices with { entry_pair = (Some a, _) } ->
		  Entry.to_string a | _ -> failwith "bug") in
	  let len = List.length choices in
	  let table = Jsutil.table (Some "tourney-outerTable") in
	  let header = Dom_html.createTr doc in
	  Jsutil.addTd header entry_str (Some "tourney-header");
	  Dom.appendChild table header;
	  let do_choice i choice =
		let () = match choice with
		  | { entry_pair = Some a, Some b; winner = Some c } ->
			let outcome = if c = a then "Defeated" else "Was defeated by" in
			let row = Dom_html.createTr doc in	
			Jsutil.addTd row outcome
			  (Some (if c = a then "tourney-won" else "tourney-lost"));
			Jsutil.addTd row (Entry.to_string b) None;
			Jsutil.addTd row ("In round " ^ (string_of_int (len - i))) None;
			Dom.appendChild table row
		  | { entry_pair = Some a, Some b; winner = None } ->
			let row = Dom_html.createTr doc in	
			Jsutil.addTd row "will play" None;
			Jsutil.addTd row (Entry.to_string b) None;
		  | { entry_pair = Some a, None; winner = None } ->
			let row = Dom_html.createTr doc in	
			Jsutil.addTd row "will play" None;
			Jsutil.addTd row "To be determined" None;
		  | _ -> failwith "bug" in
		Dom.appendChild container table;
		();
	  in
	  List.iteri do_choice choices
	in
	Array.iteri do_entry_choices by_entry

  let print_by_round tourney outer_container =
	let nonempty = List.rev (nonempty_choices tourney) in
	let num_rounds = List.length nonempty in

	let container = Jsutil.table None in
	Dom.appendChild outer_container container;

	let iter i (undecided, decided) =
	  let header = Dom_html.createTr doc in
	  Jsutil.addTd header
		(if i = 0 then
			"Finals\n"
		 else
			(if i = 1 then
				"Semifinals"
			 else
				(if i = 2 then
					"Quarterfinals"
				 else
					Printf.sprintf
					  "Round %d (%d matches)"
					  (num_rounds - i)
					  (List.length undecided + List.length decided))))
		(Some "tourney-header");
	  Dom.appendChild container header;

	  let add_choice_row i choice = 
		let row = Dom_html.createTr doc in
		(match choice with
		| { entry_pair = Some a, Some b; winner = Some c } ->
		  let loser = if c = a then b else a in
		  Jsutil.addTd row (Entry.to_string c) None;
		  Jsutil.addTd row "defeated" (Some "tourney-won");
		  Jsutil.addTd row (Entry.to_string loser) None;
		| { entry_pair = Some a, Some b; winner = None } ->
		  Jsutil.addTd row (Entry.to_string a) None;
		  Jsutil.addTd row "will face" (Some "tourney-willFace");
		  Jsutil.addTd row (Entry.to_string b) None;
		| { entry_pair = Some a, None; _ } ->
		  Jsutil.addTd row (Entry.to_string a) None;
		  Jsutil.addTd row "will face" (Some "tourney-willFace");
		  Jsutil.addTd row "(To be decided)" None;
		| _ ->
		  failwith "bug 4");
		Dom.appendChild container row in
	  let emptyRow = Dom_html.createTr doc in
	  Jsutil.addTd emptyRow " " None;
	  List.iteri add_choice_row undecided;
	  List.iteri add_choice_row decided;
	  Dom.appendChild container emptyRow
	in
	List.iteri iter nonempty


(*
  List.iteri (fun i round ->
  Printf.printf "round %d - %d undecided\n" (i + 1) (List.length round);
  List.iteri (fun i choice -> 
  match choice with
  | { entry_pair = Some a, Some b } ->
  Printf.printf "  %d: %s vs %s\n" (i + 1) (Entry.to_string a) (Entry.to_string b)
  | { entry_pair = None, Some b }->
  Printf.printf "  %d: [] vs %s\n" (i + 1) (Entry.to_string b)
  | { entry_pair = Some a, None } -> 
  Printf.printf "  %d: [] vs %s\n" (i + 1) (Entry.to_string a)
  | _ -> Printf.printf "%d: [] vs []" (i + 1))
  round)
  undecided 

*)

  let show tourney =
	let container = Jsutil.getElementById_exn "container"
	in
	let checkGroupByRounds = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
	let checkGroupByEntries = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
	let inner = Dom_html.createDiv doc in
	let top = Dom_html.createDiv doc in
	let add elt = Dom.appendChild container elt in
	let addTop elt = Dom.appendChild top elt in
	top##className <- (Js.string "tourney-menuDiv");
	add top;
	Jsutil.textNode "By Round" |> addTop;
	addTop checkGroupByRounds;
	Jsutil.textNode "By Entry" |> addTop;
	addTop checkGroupByEntries;
	add inner;
	print_by_round tourney inner;
	checkGroupByRounds##checked <- Js._true;
	let clicks positive negative print =
	(* toggle radio buttons *)
	  Lwt_js_events.clicks positive (fun event event_loop ->
		delete_children inner;
		negative##checked <- Js._false;
		Lwt.return (print tourney inner))
	in
	ignore (clicks checkGroupByRounds checkGroupByEntries print_by_round);
	ignore (clicks checkGroupByEntries checkGroupByRounds print_by_entry)

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
