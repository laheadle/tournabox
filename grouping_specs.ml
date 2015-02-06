module C = Contest
module G = Group
open Entry


let first_entry contest =
  let (first, _) = contest.C.entry_pair in
  assert (Util.is_some first);
  let slot = Util.get_option first in
  assert (Entry.is_t slot);
  match slot with
	Somebody a -> a
  | _ -> assert false

let second_slot contest = 
  snd contest.C.entry_pair

let winner_slot contest =
  contest.C.winner

let country =
object
  (* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
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
	{ Columns.header = Columns.(as_header (just_country country));
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
	Group.quit = false;
	this_group = G.Group.match_first contest group (fun p ->
	  let p = fetch p in p.country)
  }
  method extract_columns ~num_contests ~index contest =
	let open Columns in
	let a = first_entry contest in
	let round = contest.C.round in
	let columns =
	  match (second_slot contest, winner_slot contest) with
		(Some Bye, _) ->
		  [ entry a;
			advanced;
			with_a_bye;
			in_round (round + 1)]
	  | (Some (Somebody b), Some (Somebody c)) ->
		let winner, loser = if c = a then a, b else b, a in
		let outcome = if c = a then defeated ~winner loser else
			was_defeated_by ~winner loser in
		[ entry a;
		  outcome;
		  entry ~filterable:false b;
		  in_round (round + 1)]
	  | (Some (Somebody b), None) ->
		[  entry a;
		   will_face;
		   entry ~filterable:false b;
		   in_round (round + 1)]
	  | (None, None) ->
		[
		  entry a;
		  will_face;
		  to_be_decided;
		  in_round (round + 1)]
	  | _ -> failwith "bug" in
	columns
end


let contains_player lst player =
  G.Group.contains (function
  | { C.entry_pair = (Some a, Some b) } ->
	a = player || b = player
  |	{ C.entry_pair = (Some a, None) } ->
	a = player
  | { C.entry_pair = (None, Some b) } ->
	b = player
  | { C.entry_pair = (None, None) } ->
	false) lst

let contains_contest_player lst contest =
  match contest with
  | { C.entry_pair = (Some a, Some b) } ->
	(contains_player lst a) || (contains_player lst b)
  |	{ C.entry_pair = (Some a, None) } ->
	contains_player lst a
  | { C.entry_pair = (None, Some b) } ->
	contains_player lst b
  | { C.entry_pair = (None, None) } ->
	false

let round =
  (object
	method name = "By Round"
	method header_spec ~num_rounds ~num_groups ~pos:round lst =
	  let unplayed = num_rounds - num_groups in
	  let this_round = unplayed + round in
	  let header_str =
		match this_round with
		  0 -> "Finals\n"
		| 1 -> "Semifinals"
		| 2 -> "Quarterfinals"
		| _ -> (Printf.sprintf
				  "Round %d"
				  (num_rounds - this_round))
	  in
	  { Columns.header =
		  Columns.as_header
			(Columns.plain ~should_filter:false header_str);
		should_filter_header = false }
	method compare_contest a b = compare a b
	method compare_group = G.Group.compare_length_then_first
	method in_group contest group =
	  let (round_matches, already) = match G.Group.first group with
		  Some { C.round = r1; _ } ->
			(contest.C.round = r1), (contains_contest_player group contest)
		| _ -> failwith "BUG: Invalid group, by round" in
	  {
		Group.quit = round_matches && already;
		this_group = round_matches && not already
	  }
	method extract_columns ~num_contests ~index contest =
	  let open Columns in
	  let a = first_entry contest in
	  let columns =
		match (second_slot contest, winner_slot contest) with
		| Some Bye, _ ->
		  [ entry a;
			advanced;
			with_a_bye ]
		| Some (Somebody b), Some (Somebody c) ->
		  let winner, loser = if c = a then a, b else b, a in
		  [ entry c;
			defeated ~winner loser;
			entry loser ]
		| Some (Somebody b), None ->
		  [ entry a;
			will_face;
			entry b; ]
		| (None, _) ->
		  [ entry a;
			will_face;
			to_be_decided ]
		| _ ->
		  failwith "BUG: Invalid Column" in
	  columns
   end)

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
class ['a] player_group = object
  method header_spec ~(num_rounds: int) ~(num_groups: int) ~(pos: int) group =
	let header = G.Group.extract_first_first group fetch in
	{ Columns.header = Columns.(as_header (entry header));
	  should_filter_header = true; }
  method compare_contest (c1: slot C.t) (c2: slot C.t) = -(compare c1.C.round c2.C.round)
  method extract_columns ~num_contests ~index contest =
	let open Columns in
	let in_round = in_round (num_contests - index) in
	let a = first_entry contest in
	let columns =
	  match (second_slot contest, winner_slot contest) with
	  | Some Bye, _ ->
		[  advanced;
		   with_a_bye;
		   in_round ]
	  | Some (Somebody b), Some (Somebody c) ->
		let winner, loser = if c = a then a, b else b,a in
		let outcome = if c = b then was_defeated_by ~winner loser 
		  else defeated ~winner loser in
		[ outcome;
		  entry ~filterable:false b;
		  in_round ]
	  | Some (Somebody b),  None ->
		[
		  will_face;
		  entry ~filterable:false b;
		  in_round ]
	  | None, None ->
		[
		  will_face;
		  to_be_decided;
		  in_round
		]
	  | _ -> failwith "bug" in
	columns
end


class seed_group = object
	inherit [slot C.t] player_group
	method name = "By Seed";
	method compare_group g1 g2 =
	(match (G.Group.first g1), (G.Group.first g2) with
	  (Some { C.entry_pair = Some (Somebody a), _ ; _ },
	   Some { C.entry_pair = Some (Somebody b), _ ; _ }) ->
		compare_seeds a b ~if_none:(fun () ->
		  let cmp =
			compare (G.Group.length g2) (G.Group.length g1) in
		  if cmp = 0 then
			compare (to_string a) (to_string  b)
		  else cmp)
	| _ -> failwith "bad group compare")
  method in_group contest group = {
	Group.quit = false;
	this_group = G.Group.match_first contest group
	  (fun e -> let e' = fetch e in e'.seed, to_string e')
  }
  end

class performance_group = object
	inherit [slot C.t] player_group
	method name = "By Performance"
	method compare_group g1 g2 =
	  -(G.Group.compare_length_then_first g1 g2)
	method in_group contest group = {
	  Group.quit = false;
	  this_group =
		(match contest with
		  { C.entry_pair = (Some (Somebody a)), _ ; _ }
		  -> (match G.Group.first group with
			Some { C.entry_pair = (Some (Somebody b)), _ } ->
			  a = b
		  | _ -> failwith "BUG: Bad existing member")
		| _ -> failwith "BUG: Bad contest for group")
	}
   end

let performance = new performance_group

let seed = new seed_group

