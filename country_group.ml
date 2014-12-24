module C = Choice

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let o = let open Entry in object
  method header_spec ~num_rounds ~num_groups ~pos:round lst =
	let header =  C.extract_first_first lst
		(fun p ->
		  let p = fetch p in
		  match p.country with None -> assert false
		  | Some c ->
		  (* Printf.printf "%s -" c; flush_all(); *)
			try
			  let country = List.assoc c Countries.codes in
			  country
			with _ -> c)
	in
	{ Ttypes.header = Columns.(as_header (just_country header));
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
	let open Columns in
	let columns =
	  match choice with
	   { C.entry_pair = Some (Somebody a),
		  Some Bye; winner = _; round } ->
		[ entry a;
		  advanced;
		  with_a_bye;
		  in_round (round + 1)]
	  | { C.entry_pair = Some (Somebody a),
		  Some (Somebody b); winner = Some (Somebody c); round } ->
		let winner, loser = if c = a then a, b else b,a in
		let outcome = if c = a then defeated ~winner loser else
			was_defeated_by ~winner loser in
		[ entry a;
		  outcome;
		  entry ~filterable:false b;
		  in_round (round + 1)]
	  | { C.entry_pair = Some (Somebody a),
		  Some (Somebody b); winner = None ; round } ->
		[  entry a;
		   will_face;
		   entry ~filterable:false b;
		  in_round (round + 1)]
	  | { C.entry_pair = Some (Somebody a), None;
		  winner = None ; round } ->
		[
		  entry a;
		  will_face;
		  to_be_decided;
		  in_round (round + 1)]
	  | _ -> failwith "bug" in
	columns
end
