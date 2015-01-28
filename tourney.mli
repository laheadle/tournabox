type tourney
open Group

val init : Entry.slot list -> tourney

val won : tourney -> string -> tourney

val select_grouped : grouping_spec -> tourney -> GroupList.t

val num_rounds : tourney -> int

