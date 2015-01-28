
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

let column_content_string fragments =
  let f = function | Text s -> s | Elt {text} -> text in
  let strings = List.map f fragments in
  List.fold_left (^) "" strings

type header_spec = {
  header: column;
  should_filter_header: bool;
}

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
