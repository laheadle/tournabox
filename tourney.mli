
module type S = sig
  type e

  type round_in_progress = e Choice.t list

  type tourney

  val num_rounds: tourney -> int

  val entries_list: tourney -> e list
  val num_entries: tourney -> int

  val play: entries:string list -> outcomes:string list -> unit
end

module Make:
  functor (League: League.S) -> S

