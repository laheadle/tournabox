module C = Choice

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let o = let open Entry in object
  method header_spec ~num_rounds ~num_groups ~pos:round lst =
	{ Ttypes.header_str =
		C.extract_first_first lst (fun e -> to_string e);
	  should_filter_header = true; }
  method name = "By Seed";
  method compare_choice c1 c2 = -(compare c1 c2)
  method compare_group g1 g2 =
	(match g1, g2 with
	  ({ C.entry_pair = (Some a), _ ; _ } :: _,
	   { C.entry_pair = (Some b), _ ; _ } :: _) ->
		( match (a.seed, b.seed) with
		  None, None ->
			let cmp =
			  compare (List.length g2) (List.length g1) in
			if cmp = 0 then
			  compare (to_string a) (to_string b)
			else cmp
		| Some v, None -> -1
		| None, Some v -> 1
		| Some v, Some v2 -> compare v v2)
	| _ -> failwith "bad group compare")
  method in_group choice group = {
	Ttypes.quit = false;
	this_group = C.compare_first choice group
	  (fun e -> e.seed, to_string e)
  }
  method column_extractor num pos choice =
	let columns =
	  match choice with
	  | { C.entry_pair = Some a, Some b; winner = Some c } ->
		let outcome = if c = a then "Defeated" else "Was defeated by" in
		[ outcome,
		  (Some (if c = a then "tourney-won" else "tourney-lost")),
		  false;
		  (to_string b), None, false;
		  ("In round " ^ (string_of_int (num - pos))), None, false ]
	  | { C.entry_pair = Some a, Some b; winner = None } ->
		[
		  "Will face", (Some "tourney-willFace"), false;
		  (to_string b), None, false;
		  ("In round " ^ (string_of_int (num - pos))), None, false
 ]
	  | { C.entry_pair = Some a, None; winner = None } ->
		[
		  "Will face", (Some "tourney-willFace"), false;
		  "To be determined", None, false;
		  ("In round " ^ (string_of_int (num - pos))), None, false
		]
	  | _ -> failwith "bug" in
	List.map Ttypes.make_column columns
end
