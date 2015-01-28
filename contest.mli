
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

val map : ('a -> 'b) -> 'a t -> 'b t
