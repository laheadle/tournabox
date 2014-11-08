module C = Choice

type t = { player: string; country: string option; seed: int option }
type column = string * string option

let to_string entry =
  let country_str = match entry.country with None->""
	| Some x -> " " ^ x in
  let seed_str =
	(match entry.seed with None -> ""
	| Some i -> "[" ^ (string_of_int i) ^ "]") in
  Printf.sprintf "%s %s"
  (entry.player ^ country_str)
	seed_str

let of_string ?(expect_country=true) str =
  let begin_and_end regex str convert =
	  match Regexp.search regex str 0 with
		None -> assert false
	  | Some (i, result) ->
		let matched n = Regexp.matched_group result n in
		let ending = Util.map_option convert (matched 3) in
		let beginning =
		  (match matched 1 with None -> assert false | Some x -> x) in
		Util.strip_spaces beginning, ending
	in
  let ends_with_attribute_regex = Regexp.regexp "(^[^\\(]*)(\\(([a-zA-Z])\\)$)?" in
  let all_but_attribute, attribute =
	begin_and_end ends_with_attribute_regex str Util.id in
  let attribute = match attribute with None -> "" | Some s -> s in
  let ends_with_seed_regex = Regexp.regexp "(^[^\\[]*)(\\[([0-9]+)\\]$)?" in
  let all_but_seed, seed =
	begin_and_end ends_with_seed_regex all_but_attribute int_of_string in
  let get_player_with_country () =
	let name_regex = Regexp.regexp "(^.+) ([A-Z][A-Z][A-Z])$" in
	match Regexp.string_match name_regex all_but_seed 0 with
	| Some result -> 
	  (match Regexp.matched_group result 1,
		Regexp.matched_group result 2 with
		  Some x, Some y -> (x, Some y)
		| _ -> assert false)
	| None -> let _ = (failwith ("Invalid Name and Country: '" ^ all_but_seed ^"'"))
			  in "", None (* wtf ? *)
	in
  let get_player () = all_but_seed in
  let player, country =
	if expect_country then
	  get_player_with_country ()
	else
	  get_player (), None
  in
  let attribute = if attribute = "" then attribute
	else Printf.sprintf " (%s)" attribute in
  { player = player ^ attribute ; country; seed }

(* for icons see http://www.famfamfam.com/lab/icons/flags/ *)
let by_country = object
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
	let extractors =
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
	List.map Ttypes.make_column_extractor extractors
end


let by_seed = object
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
	let extractors =
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
	List.map Ttypes.make_column_extractor extractors
end

let specs = [ by_country; by_seed ]
