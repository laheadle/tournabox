

type e = Entry.t

type round_in_progress = e Choice.t list

type tourney

val num_rounds: tourney -> int

val entries_list: tourney -> e list
val num_entries: tourney -> int

val play: entries:string -> outcomes:string -> unit


