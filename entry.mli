

type t = { player: string; country: string option; seed: int option }

val to_string: t -> string
val of_string: ?expect_country:bool -> string -> t





