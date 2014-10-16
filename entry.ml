module C = Choice

module Make(Player: Player.S) =
struct
  type t = { player: Player.t; seed: int option }
  type data = int option
  type player = Player.t
  type column = string * string option

  type 'a grouping_spec = {
	name:string;
	header_name: num_rounds:int -> pos:int -> 'a Choice.t list -> string;
	compare_choice: 'a Choice.t -> 'a Choice.t -> int;
	compare_group: 'a Choice.t list -> 'a Choice.t list -> int;
	in_group: 'a Choice.t -> 'a Choice.t list -> Player.group_result;
	column_extractor: int -> int -> 'a Choice.t -> column list;
	convert: t Choice.t -> 'a Choice.t;
  }

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

	let player_specs =
	  List.map (fun pspec ->
		{ name = pspec.Player.name;
		  header_name = pspec.Player.header_name;
		  compare_choice = pspec.Player.compare_choice;
		  compare_group = pspec.Player.compare_group;
		  in_group = pspec.Player.in_group;
		  column_extractor = pspec.Player.column_extractor;
		  convert = (fun record ->
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
				| Some a -> Some a.player); }); })
		Player.grouping_specs
  end


