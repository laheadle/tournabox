module C = Choice

module M: Player.S =
  struct
	type t = { pname: string; country: string }

	let of_string str =
	  let len = String.length str in
	  let country = String.sub str (len - 3) 3 in
	  let name = String.sub str 0 (len - 3) in
	  { pname = name; country = country }

	let to_string x = x.pname ^ " " ^ x.country

	let grouping_specs = [
	  object
		method header_name ~num_rounds ~pos lst =
		  C.extract_first_first lst (fun p -> p.country)
		method name = "By Country";
		method compare_choice c1 c2 =
		  compare (C.first c1).pname (C.first c2).pname
		method compare_group =  fun g1 g2 -> -(C.compare_length_then_first g1 g2)
		method in_group choice group = {
		  Ttypes.quit = false;
		  this_group = C.compare_first choice group (fun p -> p.country)
		}
		method column_extractor num pos choice =
		  let extractors =
			match choice with
			| { C.entry_pair = Some a, Some b; winner = Some c } ->
			  let outcome = if c = a then "Defeated" else "Was defeated by" in
			  [ (to_string a), None, true;
				outcome,
				(Some (if c = a then "tourney-won" else "tourney-lost")),
				false;
				(to_string b), None, true]
			| { C.entry_pair = Some a, Some b; winner = None } ->
			  [  (to_string a), None, true;
				 "will play", None, false;
				 (to_string b), None, true ]
			| { C.entry_pair = Some a, None; winner = None } ->
			  []
			| _ -> failwith "bug" in
		  List.map Ttypes.make_column_extractor extractors
	  end
	]


  end

