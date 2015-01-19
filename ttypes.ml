
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
  method header_spec: num_rounds:int -> num_groups:int -> pos:int ->
	'a Contest.t list -> header_spec
  method compare_contest: 'a Contest.t -> 'a Contest.t -> int
  method compare_group: 'a Contest.t list -> 'a Contest.t list -> int
  method in_group: 'a Contest.t -> 'a Contest.t list -> group_result
  method column_extractor: int -> int -> 'a Contest.t -> column list
end

let is_upset columns =
  let test = function
      Text "upset" -> true
    | Text "was upset by" -> true
    | _ -> false
  in
  let is_upset frag_list =
    List.exists test frag_list
  in
  List.exists is_upset (List.map (fun col -> col.content) columns)
