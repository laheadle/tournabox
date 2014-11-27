module C = Choice

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let o = let open Entry in object
  method header_spec ~num_rounds ~num_groups ~pos:round lst =
	{ Ttypes.header_str = C.extract_first_first lst
		(fun p -> match p.country with None -> assert false
		| Some c ->
		  (* Printf.printf "%s -" c; flush_all(); *)
		  try
			let country = List.assoc c Countries.codes in
			country
		  with _ -> c
		);
	  should_filter_header = true; }
  method name = "By Country";
  method compare_choice c1 c2 =
	compare (C.first c1).player (C.first c2).player
  method compare_group =  fun g1 g2 -> -(C.compare_length_then_first g1 g2)
  method in_group choice group = {
	Ttypes.quit = false;
	this_group = C.compare_first choice group (fun p -> p.country)
  }
  method column_extractor num pos choice =
	let columns =
	  match choice with
	  | { C.entry_pair = Some a, Some b; winner = Some c; round } ->
		let outcome = if c = a then "Defeated" else "Was defeated by" in
		[ (to_string a), None, true;
		  outcome,
		  (Some (if c = a then "tourney-won" else "tourney-lost")),
		  false;
		  (to_string b), None, false;
		  "In round " ^ string_of_int (round + 1), None, false]
	  | { C.entry_pair = Some a, Some b; winner = None ; round } ->
		[  (to_string a), None, true;
		   "Will Face", (Some "tourney-willFace"), false;
		   (to_string b), None, false;
		   "In round " ^ string_of_int (round + 1), None, false ]
	  | { C.entry_pair = Some a, None; winner = None ; round } ->
		[
		  (to_string a), None, true;
		  "Will face", (Some "tourney-willFace"), false;
		  "To Be Determined", None, false;
		  "In round " ^ string_of_int (round + 1), None, false
		]
	  | _ -> failwith "bug" in
	List.map Ttypes.make_column columns
end
