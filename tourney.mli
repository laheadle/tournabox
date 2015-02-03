
(** The fundamental type of a tournament *)
type tourney

(** Make a tournament from some entries. *)
val make : Entry.slot list -> tourney

(** Record a winner. *)
val won : tourney -> string -> tourney

(** Group the results. *)
val select_grouped : Group.grouping_spec -> tourney -> Group.GroupList.t

(** Get the number of rounds in a tournament. *)
val num_rounds : tourney -> int

