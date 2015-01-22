
type group_result =  {
  quit: bool;
  this_group: bool;
}

(* non-nested html span *)
type shallow_span = {
  class_name: string;
  text: string;
}

type column_fragment =
  Text of string
| Elt of shallow_span

type column = {
  content: column_fragment list;
  class_name: string option;
  should_filter: bool;
}

val column_content_string: column_fragment list -> string

val is_upset: column list -> bool

type header_spec = {
  header: column;
  should_filter_header: bool;
}

