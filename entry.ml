module C = Choice

module Make(Player: Player.S) =
struct
  type t = { player: Player.t; seed: int option }
  type data = int option
  type player = Player.t
  type column = string * string option

	let make player data =
	  { player = player; seed = data }
		
	let to_string entry = Printf.sprintf "%s %s"
	  (Player.to_string entry.player)
	  (match entry.seed with None -> ""
	  | Some i -> "[" ^ (string_of_int i) ^ "]")

	let parse str =
	  let name_regex = Regexp.regexp "(^.+) [A-Z][A-Z][A-Z]" in
	  let seed_regex = Regexp.regexp "\\[([0-9]+)\\]$" in

	  let get_seed str =
		match Regexp.search_forward seed_regex str 0 with
		  None -> raise Not_found
		| Some (i, result) -> int_of_string
		  (match Regexp.matched_group result 1 with None -> raise Not_found | Some x -> x)
	  in
	  let get_name str =
		match Regexp.string_match name_regex str 0 with
		  None -> raise Not_found
		| Some result -> (match Regexp.matched_group result 1 with None -> raise Not_found | Some x -> x)
		  
	  in
	  let seed =
		try
		  Some (get_seed str)
		with Not_found ->
		  None
	  in
	  let name = get_name str in
	  (name, seed)

	let entry_specs =
	  [
	  object
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
			(fun e -> (e.seed, Player.to_string e.player))
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
		method convert x = x
	  end
	]

	let player_specs =
	  List.map (fun pspec ->
				object
				  method name = pspec#name
				  method header_name = pspec#header_name
				  method compare_choice = pspec#compare_choice
				  method compare_group = pspec#compare_group
				  method in_group = pspec#in_group
				  method column_extractor = pspec#column_extractor
				  method convert record =
					{ C.round = record.C.round;
					  position = record.C.position;
					  entry_pair =
						(match record.C.entry_pair with
						  None, None -> None, None
						| Some a, None -> Some a.player, None
						| None, Some b -> None, Some b.player
						| Some a, Some b -> Some a.player, Some b.player );
					  winner =
						(match record.C.winner with
						  None -> None
						| Some a -> Some a.player); }
				end)
		Player.grouping_specs
end


