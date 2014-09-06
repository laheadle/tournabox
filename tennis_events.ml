let make_entries db lst =
  let name_regex = Str.regexp "\\(^.+\\) [A-Z][A-Z][A-Z]" in
(*  let seed_regex = Str.regexp "\\[\\(0-9+\\)\\]$" in*)
  let seed_regex = Str.regexp "\\[\\([0-9]+\\)\\]$" in

  let get_seed str =
	let _ = Str.search_forward seed_regex str 0 in
	int_of_string (Str.matched_group 1 str)
  in
  let get_name str =
	let matched = Str.string_match name_regex str 0 in
	if matched then
	  Str.matched_group 1 str
	else
	  raise Not_found
  in
  let entry str =
	let seed = try
				 Some (get_seed str)
	  with Not_found ->
		None
	in
	let name = get_name str in
(*	let () = match (name, seed) with
		str, None -> Printf.printf "%s none" str
	  | 	  str, Some i -> Printf.printf "%s [%d]" str i in *)
	Tennis_player_entry.of_string name db seed
  in
  List.map entry lst


