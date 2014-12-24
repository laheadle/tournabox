
type group_result =  {
  quit: bool;
  this_group: bool;
}


(* non-nested html tag *)
type shallow_elt = {
  tag: string;
  class_name: string;
  text: string;
}

type column_fragment =
  Text of string
| Elt of shallow_elt

type column = {
  content: column_fragment list;
  class_name: string option;
  should_filter: bool;
}

let column_content_string fragments =
  let f = function | Text s -> s | Elt {text} -> text in
  let strings = List.map f fragments in
  List.fold_left (^) "" strings

type header_spec = {
  header: column;
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



