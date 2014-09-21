
module type S = sig
  type e

  type choice = {
	entry_pair : e option * e option;
	winner : e option;
  }

  type round_in_progress = choice list

  type tourney

  val init : e list -> tourney

  val num_rounds: tourney -> int

  val undecided_choices: tourney -> round_in_progress list
  val decided_choices: tourney -> round_in_progress list

  val entries: tourney -> e list
  val num_entries: tourney -> int

  val won: tourney -> e -> tourney
  val won_str : tourney -> string -> tourney

  val show: tourney -> unit
  val play: e list -> string list -> unit
end

module Make:
  functor (E: Entry.S) ->
      S with type e = E.t

