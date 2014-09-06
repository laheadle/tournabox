type t = { player: Tennis_player.t; seed: int option }

let of_string str db seed =
  { player = (Tennis_player.pick str db); seed = seed }

let to_string entry = entry.player.Tennis_player.name ^ 
	(match entry.player.Tennis_player.country, entry.seed with
	  c, None -> "[" ^ c ^ "]"
	| c, Some s -> ("[" ^ c ^ ", " ^ (string_of_int s) ^ "]"))
