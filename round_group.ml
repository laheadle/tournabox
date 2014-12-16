module C = Choice

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

let o =
  let open Entry in
  (object
	method name = "By Round"
	method header_spec ~num_rounds ~num_groups ~pos:round lst =
	  let len = List.length lst in
	  let unplayed = num_rounds - num_groups in
	  let this_round = unplayed + round in
	  let header_str =
		match this_round with
		  0 -> "Finals\n"
		| 1 -> "Semifinals"
		| 2 -> "Quarterfinals"
		| _ -> (Printf.sprintf
				  "Round %d (%d matches)"
				  (num_rounds - this_round)
				  len)
	  in
	  { Ttypes.header_str; should_filter_header = false }
	method compare_choice a b = compare a b
	method compare_group = C.compare_length_then_first
	method in_group choice group =
	  let (round_matches, already) = match group with
		  { C.round = r1; _ } :: _ ->
			(choice.C.round = r1), (contains_choice_player group choice)
		| _ -> failwith "BUG: Invalid group, by round" in
	  {
		Ttypes.quit = round_matches && already;
		this_group = round_matches && not already
	  }
	method column_extractor num pos choice =
	  let columns = match choice with
		| { C.entry_pair = Some (Somebody a),
			Some Bye; winner = _ } ->
		  [ (Entry.to_string a), None, true;
			"advanced", (Some "tournabox-won"), false;
			"with a bye", None, true ]
		| { C.entry_pair = Some (Somebody a),
			Some (Somebody b); winner = Some (Somebody c) } ->
		  let loser = if c = a then b else a in
		  [ (Entry.to_string c), None, true;
			"defeated", (Some "tournabox-won"), false;
			(Entry.to_string loser), None, true ]
		| { C.entry_pair = Some (Somebody a),
			Some (Somebody b); winner = None } ->
		  [ (Entry.to_string a), None, true;
			"will face", (Some "tournabox-willFace"), false;
			(Entry.to_string b), None, true ]
		| { C.entry_pair = Some (Somebody a), None; _ } ->
		  [ (Entry.to_string a), None, true;
			"will face", (Some "tournabox-willFace"), false;
			"(To be decided)", None, false ]
		| _ ->
		  failwith "BUG: Invalid Column" in
	  List.map Ttypes.make_column columns
   end)
