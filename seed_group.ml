module C = Choice

let in_round num pos =
  ("In round " ^ (string_of_int (num - pos))), None, false

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let o = let open Entry in object
  method header_spec ~num_rounds ~num_groups ~pos:round lst =
	{ Ttypes.header_str =
		C.extract_first_first lst (function | Somebody e -> to_string e | Bye -> assert false);
	  should_filter_header = true; }
  method name = "By Seed";
  method compare_choice c1 c2 = -(compare c1.C.round c2.C.round)
  method compare_group g1 g2 =
	(match g1, g2 with
	  ({ C.entry_pair = Some (Somebody a), _ ; _ } :: _,
	   { C.entry_pair = Some (Somebody b), _ ; _ } :: _) ->
		( match (a.seed, b.seed) with
		  None, None ->
			let cmp =
			  compare (List.length g2) (List.length g1) in
			if cmp = 0 then
			  compare (to_string a) (to_string  b)
			else cmp
		| Some v, None -> -1
		| None, Some v -> 1
		| Some v, Some v2 -> compare v v2)
	| _ -> failwith "bad group compare")
  method in_group choice group = {
	Ttypes.quit = false;
	this_group = C.compare_first choice group
	  (function | Somebody e -> e.seed, to_string e | Bye -> assert false);
  }

  method column_extractor num pos choice =
	let in_round = in_round num pos in
	let columns =
	  match choice with
	  | { C.entry_pair = Some _, Some Bye; winner = _ } ->
		[  "Advanced", (Some "tourney-won"), false;
		   "With a Bye", None, false;
		   in_round ]
	  | { C.entry_pair =
		  Some _,
		  Some (Somebody b);
		  winner = Some (Somebody c) } ->
		let outcome = if c = b then "Was defeated by" else "Defeated" in
		[ outcome,
		  (Some (if c = b then "tourney-lost" else "tourney-won")),
		  false;
		  (to_string b), None, false;
		  in_round ]
	  | { C.entry_pair = Some (Somebody a), Some (Somebody b);
		  winner = None } ->
		[
		  "Will face", (Some "tourney-willFace"), false;
		  (to_string b), None, false;
		  in_round ]
	  | { C.entry_pair = Some (Somebody a),None;
		  winner = None } ->
		[
		  "Will face", (Some "tourney-willFace"), false;
		  "To be determined", None, false;
		  in_round
		]
	  | _ -> failwith "bug" in
	List.map Ttypes.make_column columns
end
