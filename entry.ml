module C = Contest

type t = { name: string; country: string option; seed: int option }
type column = string * string option

let get_seed t = t.seed

let get_country t = t.country

let get_name t = t.name

let to_string entry =
  let country_str = match entry.country with None->""
                                           | Some x -> " " ^ x in
  let seed_str =
    (match entry.seed with None -> ""
                         | Some i -> "[" ^ (string_of_int i) ^ "]") in
  Printf.sprintf "%s%s"
    (entry.name ^ country_str)
    seed_str

let t_of_string ?(expect_country=true) str =
  let begin_and_end regex str convert =
    match Regexp.search regex str 0 with
      None -> assert false
    | Some (i, result) ->
      let matched n = Regexp.matched_group result n in
      let ending = Util.map_option convert (matched 3) in
      let beginning =
        (match matched 1 with None -> assert false | Some x -> x) in
      Util.strip_spaces beginning, ending
  in
  let ends_with_attribute_regex = Regexp.regexp "(^[^\\(]*)(\\(([a-zA-Z])\\)$)?" in
  let all_but_attribute, attribute =
    begin_and_end ends_with_attribute_regex str Util.id in
  let attribute = match attribute with None -> "" | Some s -> s in
  let ends_with_seed_regex = Regexp.regexp "(^[^\\[]*)(\\[([0-9]+)\\]$)?" in
  let all_but_seed, seed =
    begin_and_end ends_with_seed_regex all_but_attribute int_of_string in
  let get_name_with_country () =
    let name_regex = Regexp.regexp "(^.+) ([A-Z][A-Z][A-Z])$" in
    match Regexp.string_match name_regex all_but_seed 0 with
    | Some result -> 
      (match Regexp.matched_group result 1,
             Regexp.matched_group result 2 with
        Some x, Some y -> (x, Some y)
      | _ -> assert false)
    | None -> let _ = (failwith ("Invalid Name and Country: '" ^ all_but_seed ^"'"))
      in "", None (* wtf ? *)
  in
  let get_name () = all_but_seed in
  let name, country =
    if expect_country then
      get_name_with_country ()
    else
      get_name (), None
  in
  let attribute = if attribute = "" then attribute
    else Printf.sprintf " (%s)" attribute in
  { name = name ^ attribute ; country; seed }

type slot = Bye | Somebody of t

let slot_of_string ?(expect_country=true) str =
  if str = "-bye-" then Bye else
    Somebody (t_of_string ~expect_country str)

let fetch = function
  | Bye -> assert false
  | Somebody t -> t

let is_bye = function
  | Bye -> true
  | Somebody _ -> false

let is_t = function
  | Bye -> false
  | Somebody _ -> true


let compare_seeds a b ~if_none =
  match (a.seed, b.seed) with
    None, None -> if_none ()
  | Some v, None -> -1
  | None, Some v -> 1
  | Some v, Some v2 -> compare v v2
