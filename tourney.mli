type tourney

type contest = Entry.slot Contest.t

type group = contest list

class type grouping_spec = object
  method name:string
  method header_spec: num_rounds:int -> num_groups:int -> pos:int -> group -> Ttypes.header_spec
  method compare_contest: contest -> contest -> int
  method compare_group: contest list -> contest list -> int
  method in_group: contest -> contest list -> Ttypes.group_result
  method column_extractor: int -> int -> contest -> Ttypes.column list
end

val init : Entry.slot list -> tourney

val won : tourney -> string -> tourney

val select_grouped : grouping_spec -> tourney -> group list

val num_rounds : tourney -> int

