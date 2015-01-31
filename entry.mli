
(** An entry who may win or lose against another Entry.t, but always
	beats a bye. *)
type t = { player: string; country: string option; seed: int option }

val to_string: t -> string

(** A slot in the tournament. May be a Somebody or a Bye slot, which
	always loses. *)
type slot = Bye | Somebody of t

(** Parse a string to a slot *)
val slot_of_string: ?expect_country:bool -> string -> slot

(** Expects a Somebody. Assertion failure on Bye *)
val fetch: slot -> t

val is_bye: slot -> bool

val is_t: slot -> bool

(** Compare the seeds of two t's so that the lower seed comes
	first. If neither has a seed, then call if_none *)
val compare_seeds: t -> t -> if_none:(unit -> int) -> int



