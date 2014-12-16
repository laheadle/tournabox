module C = Choice

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let o = let open Entry in object
  method header_spec ~num_rounds ~num_groups ~pos:round lst =
	{ Ttypes.header_str = C.extract_first_first lst
		(fun p ->
		  let p = fetch p in
		  match p.country with None -> assert false
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
	let player choice =
	  choice |> C.first |> fetch |> (fun c -> c.player) in
	compare (player c1) (player c2) 

  method compare_group =  fun g1 g2 -> -(C.compare_length_then_first g1 g2)
  method in_group choice group = {
	Ttypes.quit = false;
	this_group = C.compare_first choice group (fun p ->
	  let p = fetch p in p.country)
  }
  method column_extractor num pos choice =
	let columns =
	  match choice with
	   { C.entry_pair = Some (Somebody a),
		  Some Bye; winner = _; round } ->
		[ (to_string a), None, true;
		  "Advanced", Some "tournabox-won", false;
		  "with a bye", None, false;
		  "In round " ^ string_of_int (round + 1), None, false]
	  | { C.entry_pair = Some (Somebody a),
		  Some (Somebody b); winner = Some (Somebody c); round } ->
		let outcome = if c = a then "Defeated" else "Was defeated by" in
		[ (to_string a), None, true;
		  outcome,
		  (Some (if c = a then "tournabox-won" else "tournabox-lost")),
		  false;
		  (to_string b), None, false;
		  "In round " ^ string_of_int (round + 1), None, false]
	  | { C.entry_pair = Some (Somebody a),
		  Some (Somebody b); winner = None ; round } ->
		[  (to_string a), None, true;
		   "Will Face", (Some "tournabox-willFace"), false;
		   (to_string b), None, false;
		   "In round " ^ string_of_int (round + 1), None, false ]
	  | { C.entry_pair = Some (Somebody a), None;
		  winner = None ; round } ->
		[
		  (to_string a), None, true;
		  "Will face", (Some "tournabox-willFace"), false;
		  "To Be Determined", None, false;
		  "In round " ^ string_of_int (round + 1), None, false
		]
	  | _ -> failwith "bug" in
	List.map Ttypes.make_column columns
end
