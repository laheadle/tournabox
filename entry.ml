
module type S =
  sig
	type t
	type db
	val to_string: t -> string
	val make_entries: db -> string list -> t list
  end

module Make(Player: Player.S) =
  struct
	type t = { player: Player.t; seed: int option }
	type db = Player.db

	let of_string str db seed =
	  { player = (Player.pick str db); seed = seed }
		
	let to_string entry = Printf.sprintf "%s %s"
	  (Player.to_string entry.player)
	  (match entry.seed with None -> ""
	  | Some i -> "[" ^ (string_of_int i) ^ "]")

	let make_entries db lst =
  (*  let name_regex = Regexp.regexp "\\(^.+\\) [A-Z][A-Z][A-Z]" in
	  let seed_regex = Regexp.regexp "\\[\\([0-9]+\\)\\]$" in
  *)
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
	  let entry str =
		let seed =
		  try
			Some (get_seed str)
		  with Not_found ->
			None
		in
		let name = get_name str in
		of_string name db seed
	  in
	  List.map entry lst

  end


