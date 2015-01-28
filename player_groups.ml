module C = Contest
module G = Group

open Entry

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
class ['a] parent = object
  method header_spec ~(num_rounds: int) ~(num_groups: int) ~(pos: int) lst =
	let header = G.Group.extract_first_first lst fetch in
	{ Ttypes.header = Columns.(as_header (entry header));
	  should_filter_header = true; }
  method compare_contest (c1: slot C.t) (c2: slot C.t) = -(compare c1.C.round c2.C.round)
  method column_extractor num pos contest =
	let open Columns in
	let in_round = in_round (num - pos) in
	let columns =
	  match contest with
	  | { C.entry_pair = Some _, Some Bye; winner = _ } ->
		[  advanced;
		   with_a_bye;
		   in_round ]
	  | { C.entry_pair =
		  Some (Somebody a),
		  Some (Somebody b);
		  winner = Some (Somebody c) } ->
		let winner, loser = if c = a then a, b else b,a in
		let outcome = if c = b then was_defeated_by ~winner loser 
		  else defeated ~winner loser in
		[ outcome;
		  entry ~filterable:false b;
		  in_round ]
	  | { C.entry_pair = Some (Somebody a), Some (Somebody b);
		  winner = None } ->
		[
		  will_face;
		  entry ~filterable:false b;
		  in_round ]
	  | { C.entry_pair = Some (Somebody a),None;
		  winner = None } ->
		[
		  will_face;
		  to_be_decided;
		  in_round
		]
	  | _ -> failwith "bug" in
	columns
end


class seed_group = object
	inherit [slot C.t] parent
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
	Ttypes.quit = false;
	this_group = G.Group.compare_first contest group
	  (function | Somebody e -> e.seed, to_string e | Bye -> assert false);
  }
  end

class performance_group = object
	inherit [slot C.t] parent
	method name = "By Performance"
	method compare_group g1 g2 =
	  -(G.Group.compare_length_then_first g1 g2)
	method in_group contest group = {
	  Ttypes.quit = false;
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
