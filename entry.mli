
type t = { player: string; country: string option; seed: int option }

val to_string: t -> string

type slot = Bye | Somebody of t

val slot_of_string: ?expect_country:bool -> string -> slot

val fetch: slot -> t
val is_bye: slot -> bool
val is_t: slot -> bool




