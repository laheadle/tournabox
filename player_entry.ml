
type t = { player: Player.t; seed: int }

let of_string str db =
  Player.pick str db
