type tourney

type contest = Entry.slot Contest.t

type group = contest list

type grouping_spec = Entry.slot Ttypes.grouping_spec

val init : Entry.slot list -> tourney

val won : tourney -> string -> tourney

val select_grouped : grouping_spec -> tourney -> group list

val num_rounds : tourney -> int

