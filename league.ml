module type Raw_Players = sig val v: string list end

module type S = sig
  module Player: Player.S
  module Raw_Players: Raw_Players
  type db
  val make_db: unit -> db
  val pick: string -> db -> Player.t
end

module Make(Player: Player.S) (Raw_Players: Raw_Players) =
struct
  module Player = Player
  module Raw_Players = Raw_Players
  type db = Player.t list
  let make_db () = List.map Player.of_string Raw_Players.v
  let pick str db = Util.pick db str Player.to_string
end 
