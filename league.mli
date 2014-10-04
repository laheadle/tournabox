module type Raw_Players = sig val v: string list end

module type S = sig
  module Player: Player.S
  module Raw_Players: Raw_Players
  type db
  val make_db: unit -> db
  val pick: string -> db -> Player.t
end

module Make:
  functor (Player: Player.S) ->
	functor (Raw_Players: Raw_Players) -> S
