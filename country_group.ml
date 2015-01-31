module C = Contest
module G = Group

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let o = let open Entry in object
  method header_spec ~num_rounds ~num_groups ~pos:round group =
	let get_country entry =
	  let entry = fetch entry in
	  match entry.country with None -> assert false
	  | Some c ->
		try
		  let country = List.assoc c Countries.codes in
		  country
		with _ -> c
	in
	let country =  G.Group.extract_first_first group get_country
	in
	{ Ttypes.header = Columns.(as_header (just_country country));
	  should_filter_header = true; }
  method name = "By Country";
  method compare_contest c1 c2 =
	let entry contest =
	  contest |> C.first |> fetch in
	let player contest =
	  contest |> C.first |> fetch |> (fun c -> c.player) in
	compare_seeds (entry c1) (entry c2) ~if_none: (fun () ->
	  compare (player c1) (player c2) )

  method compare_group =  fun g1 g2 -> -(G.Group.compare_length_then_first g1 g2)
  method in_group contest group = {
	Ttypes.quit = false;
	this_group = G.Group.match_first contest group (fun p ->
	  let p = fetch p in p.country)
  }
  method column_extractor num pos contest =
	let open Columns in
	let columns =
	  match contest with
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
