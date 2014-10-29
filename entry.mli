

type t = { player: string; country: string; seed: int option }

val to_string: t -> string
val of_string: string -> t

val specs: t Ttypes.grouping_spec list



