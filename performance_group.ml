module C = Choice

let o =
  (object
	method name = "By Performance"
	method header_spec ~num_rounds ~num_groups ~pos:round lst =
	  let header_str = C.extract_first_first lst (fun e -> Entry.to_string e) in
	  { Ttypes.header_str; should_filter_header=true }
	method compare_choice c1 c2 = -(compare c1 c2)
	method compare_group =
	  fun g1 g2 -> -(C.compare_length_then_first g1 g2)
	method in_group choice group = {
	  Ttypes.quit = false;
	  this_group =
		(match choice with
		  { C.entry_pair = (Some a), _ ; _ }
		  -> (match group with
			{ C.entry_pair = (Some b), _ } :: _ ->
			  a = b
		  | _ -> failwith "BUG: Bad existing member")
		| _ -> failwith "BUG: Bad choice for group")
	}
	method column_extractor num pos choice =
	  let extractors =
		match choice with
		| { C.entry_pair = Some a, Some b; winner = Some c } ->
		  let outcome = if c = a then "Defeated" else "Was defeated by" in
		  [ outcome,
			Some (if c = a then "tourney-won" else "tourney-lost"),
			false;
			(Entry.to_string b), None, false;
			("In round " ^ (string_of_int (num - pos))), None, false ]
		| { C.entry_pair = Some a, Some b; winner = None } ->
		  [ "Will face", Some "tourney-willFace", false;
			(Entry.to_string b), None, false]
		| { C.entry_pair = Some a, None; winner = None } ->
		  [ "Will face", Some "tourney-willFace", false;
			"To be determined", None, false ]
		| _ -> failwith "BUG: Invalid Column" in
	  List.map Ttypes.make_column_extractor extractors
   end)
