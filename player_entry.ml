
type t = { player: Player.t; seed: int option }

let of_string str db seed =
  { player = (Player.pick str db); seed = seed }

let to_string entry = entry.player.Player.name ^ 
	(match entry.player.Player.country, entry.seed with
	  c, None -> "[" ^ c ^ "]"
	| c, Some s -> ("[" ^ c ^ ", " ^ (string_of_int s) ^ "]"))
