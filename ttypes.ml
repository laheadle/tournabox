
type group_result =  {
  quit: bool;
  this_group: bool;
}

type column = {
  content: string;
  class_name: string option;
  should_filter: bool;
}

type header_spec = {
  header_str: string;
  should_filter_header: bool;
}

class type ['a] grouping_spec = object
  method name:string
  method header_spec: num_rounds:int -> num_groups:int -> pos:int -> 'a Choice.t list -> header_spec
  method compare_choice: 'a Choice.t -> 'a Choice.t -> int
  method compare_group: 'a Choice.t list -> 'a Choice.t list -> int
  method in_group: 'a Choice.t -> 'a Choice.t list -> group_result
  method column_extractor: int -> int -> 'a Choice.t -> column list
end


let make_column_extractor (content, class_name, should_filter) = 
  { content; class_name; should_filter}

