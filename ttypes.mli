
type group_result =  {
  quit: bool;
  this_group: bool;
}

type column = {
  content: string;
  class_name: string option;
  should_filter: bool;
}

val make_column_extractor: string * string option * bool -> column

class type ['a] grouping_spec = object
  method name:string
  method header_name: num_rounds:int -> pos:int -> 'a Choice.t list -> string
  method compare_choice: 'a Choice.t -> 'a Choice.t -> int
  method compare_group: 'a Choice.t list -> 'a Choice.t list -> int
  method in_group: 'a Choice.t -> 'a Choice.t list -> group_result
  method column_extractor: int -> int -> 'a Choice.t -> column list
end

class type ['a, 'b] converted_grouping_spec = object
  inherit ['a] grouping_spec
  method convert: 'b Choice.t -> 'a Choice.t
end
