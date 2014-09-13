let make_entries db lst =
(*  let name_regex = Regexp.regexp "\\(^.+\\) [A-Z][A-Z][A-Z]" in
  let seed_regex = Regexp.regexp "\\[\\([0-9]+\\)\\]$" in
*)
  let name_regex = Regexp.regexp "(^.+) [A-Z][A-Z][A-Z]" in
  let seed_regex = Regexp.regexp "\\[([0-9]+)\\]$" in

  let get_seed str =
	match Regexp.search_forward seed_regex str 0 with
	  None -> raise Not_found
	| Some (i, result) -> int_of_string
	  (match Regexp.matched_group result 1 with None -> raise Not_found | Some x -> x)
  in
  let get_name str =
	match Regexp.string_match name_regex str 0 with
	  None -> raise Not_found
	| Some result -> (match Regexp.matched_group result 1 with None -> raise Not_found | Some x -> x)
	
  in
  let entry str =
	let seed =
	  try
		Some (get_seed str)
	  with Not_found ->
		None
	in
	let name = get_name str in
	Tennis_player_entry.of_string name db seed
  in
  List.map entry lst


