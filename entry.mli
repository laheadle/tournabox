(** Each Entry in a tournament takes up a slot. *)

(** An entry who may win or lose against another Entry.t, but always
    	beats a bye. *)
type t

(** Get the optional seed *)
val get_seed: t -> int option

(** Get the optional country *)
val get_country: t -> string option

(** Get the name *)
val get_name: t -> string

val to_string: t -> string

(** A slot in the tournament. May be a Somebody, which can win or
    	lose, or a Bye, which always loses. *)
type slot = Bye | Somebody of t

(** Parse a string to a slot *)
val slot_of_string: ?expect_country:bool -> string -> slot

(** Expects a Somebody. Assertion failure on Bye *)
val fetch: slot -> t

(** True of Bye slots *)
val is_bye: slot -> bool

(** True of Somebody slots *)
val is_t: slot -> bool

(** Compare the seeds of two t's so that the lower seed comes
    	first. If neither has a seed, then call if_none *)
val compare_seeds: t -> t -> if_none:(unit -> int) -> int



