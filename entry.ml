module C = Choice

type t = { player: string; country: string; seed: int option }
type column = string * string option

let to_string entry = Printf.sprintf "%s %s"
  (entry.player ^ " " ^ entry.country)
  (match entry.seed with None -> ""
  | Some i -> "[" ^ (string_of_int i) ^ "]")

let of_string str =
  let name_regex = Regexp.regexp "(^.+) ([A-Z][A-Z][A-Z])" in
  let seed_regex = Regexp.regexp "\\[([0-9]+)\\]$" in

  let get_seed str =
	match Regexp.search_forward seed_regex str 0 with
	  None -> raise Not_found
	| Some (i, result) -> int_of_string
	  (match Regexp.matched_group result 1 with None -> raise Not_found | Some x -> x)
  in
  let parse_name str =
	match Regexp.string_match name_regex str 0 with
	  None -> raise Not_found
	| Some result -> 
	  match Regexp.matched_group result 1,
		Regexp.matched_group result 2 with
		 Some x, Some y -> x, y
		| _ -> raise Not_found
  in
  let seed =
	try
	  Some (get_seed str)
	with Not_found ->
	  None
  in
  (*  Printf.printf "Parse %s" str; flush_all(); *)
  let player, country = parse_name str in
  (*  Printf.printf "Player %s" player; flush_all(); *)
  { player; country; seed }

let by_country = object
  method header_name ~num_rounds ~pos lst =
	C.extract_first_first lst (fun p -> p.country)
  method name = "By Country";
  method compare_choice c1 c2 =
	compare (C.first c1).player (C.first c2).player
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


let by_seed = object
  method header_name ~num_rounds ~pos lst =
	C.extract_first_first lst (fun e -> to_string e)
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
	  (fun e -> (e.seed, to_string e))
  }
  method column_extractor num pos choice =
	let extractors =
	  match choice with
	  | { C.entry_pair = Some a, Some b; winner = Some c } ->
		let outcome = if c = a then "Defeated" else "Was defeated by" in
		[ outcome,
		  (Some (if c = a then "tourney-won" else "tourney-lost")),
		  false;
		  (to_string b), None, true]
	  | { C.entry_pair = Some a, Some b; winner = None } ->
		[ 
		  "will play", None, false;
		  (to_string b), None, true ]
	  | { C.entry_pair = Some a, None; winner = None } ->
		[]
	  | _ -> failwith "bug" in
	List.map Ttypes.make_column_extractor extractors
end

let specs = [ by_country; by_seed ]
