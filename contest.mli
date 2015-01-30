
(** A contest is a decision point in a tournament, when one of two
	contestants is eliminated from the tournament, and the other one moves
	on to the next round (or wins the tournament). *)
type 'a t = {

  (** The round during which the contest took place (zero-indexed). *)
  round : int;

  (** At what position in the round (zero-indexed). There are as many
	  positions as contests in a round, and twice as many contestants
	  as positions in a round. *)
  position : int;

  (** The contending contestants *)
  entry_pair : 'a option * 'a option;

  (** The winner of the contest, or None if not completed. *)
  winner : 'a option;
}

(** The first contestant *)
val first : 'a t -> 'a

(** 
	Conversion to a different type of contestant.

	This is used to convert between int t and Entry.slot t.

	The int representation is used for calculating paths through the
	tournament, and thus for figuring out who has lost to a given
	winner. For example, in the first round, contestants 2 and 3 vie
	for position 1; in the second round, the winner will vie for
	position 0. Thus, given an int, we can predict its path through
	the tournament using a simple calculation, e.g. 4 -> 2 -> 1 ->
	0. See function path_intersect.

	The Entry.slot representation is used for printing and sorting of
	results.
*)
val map : ('a -> 'b) -> 'a t -> 'b t
