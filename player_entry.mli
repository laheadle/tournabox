
type t = { player: Player.t; seed: int option }

val of_string: string -> Player.db -> int option -> t

val to_string: t -> string
