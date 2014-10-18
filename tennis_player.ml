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
		  match lst with
			choice :: tl -> 
			  (C.first choice).country
		method name = "By Country";
		method compare_choice c1 c2 =
		  compare (C.first c1).pname (C.first c2).pname
		method compare_group =  C.compare_length_then_first
		method in_group choice group = {
		  Ttypes.quit = false;
		  this_group = C.compare_first choice group (fun p -> p.country)
		}
		method column_extractor num pos choice =
		  match choice with
		  | { C.entry_pair = Some a, Some b; winner = Some c } ->
			let outcome = if c = a then "Defeated" else "Was defeated by" in
			[ (to_string a), None;
			  outcome,
			  (Some (if c = a then "tourney-won" else "tourney-lost"));
			  (to_string b), None]
		  | { C.entry_pair = Some a, Some b; winner = None } ->
			[  (to_string a), None;
			  "will play", None;
			  (to_string b), None ]
		  | { C.entry_pair = Some a, None; winner = None } ->
			[]
		  | _ -> failwith "bug"
	  end
	]


  end

