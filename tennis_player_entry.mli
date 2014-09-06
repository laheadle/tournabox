
type t = { player: Tennis_player.t; seed: int option }

val of_string: string -> Tennis_player.db -> int option -> t

val to_string: t -> string
