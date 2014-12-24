module C = Choice

let o =
  let open Entry in
  (object
	method name = "By Performance"
	method header_spec ~num_rounds ~num_groups ~pos:round lst =
	  let header = C.extract_first_first lst fetch in
	  { Ttypes.header = Columns.(as_header (entry header));
		should_filter_header=true }
	method compare_choice c1 c2 = -(compare c1 c2)
	method compare_group =
	  fun g1 g2 -> -(C.compare_length_then_first g1 g2)
	method in_group choice group = {
	  Ttypes.quit = false;
	  this_group =
		(match choice with
		  { C.entry_pair = (Some (Somebody a)), _ ; _ }
		  -> (match group with
			{ C.entry_pair = (Some (Somebody b)), _ } :: _ ->
			  a = b
		  | _ -> failwith "BUG: Bad existing member")
		| _ -> failwith "BUG: Bad choice for group")
	}
	method column_extractor num pos choice =
	  let open Columns in
	  let in_round = in_round (num - pos) in
	  let columns =
		match choice with
		| { C.entry_pair = Some (Somebody a), Some Bye; winner = _ } ->
		  [ advanced;
			with_a_bye;
			in_round;]
		| { C.entry_pair = Some (Somebody a),
			Some (Somebody b); winner = Some (Somebody c) } ->
		  let winner, loser = if c = a then a, b else b,a in
		  let outcome = if c = a then defeated ~winner loser else was_defeated_by ~winner loser in
		  [ outcome;
			entry  ~filterable:false b;
			in_round]
		| { C.entry_pair = Some (Somebody a),
			Some (Somebody b); winner = None } ->
		  [ will_face;
			entry  ~filterable:false b;
			in_round]
		| { C.entry_pair = Some (Somebody a),
			None; winner = None } ->
		  [ will_face;
			to_be_decided;
			in_round]
		| _ -> failwith "BUG: Invalid Column" in
	  columns
   end)
