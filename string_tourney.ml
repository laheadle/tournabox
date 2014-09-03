let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let pick tourney partial =
  let up = String.uppercase in
  let matching = List.filter (fun player ->
	contains (up player) (up partial)) (Tourney.players tourney) in
  if List.length matching = 1 then
	List.hd matching
  else
	raise (Invalid_argument
			 (List.fold_left 
				(fun strs player ->
				  strs ^ player ^ "; ")
				("bad player: " ^ partial ^ " could be: ")
				matching))

let won tourney partial =
  let player = pick tourney partial in
  Tourney.won tourney player

let print_tourney tourney =
  let undecided = Tourney.undecided_choices tourney in
  let decided = Tourney.decided_choices tourney in

  List.iteri (fun i round ->
	Printf.printf "round %d - %d decided\n" (i + 1) (List.length round);
	List.iteri (fun i choice -> 
	  match choice with
	  | { Tourney.player_pair = Some a, Some b; Tourney.winner = Some c } ->
		Printf.printf "  %d: %s vs %s - winner: %s\n" i a b c
	  | _ -> failwith "bug")
	  round)
	decided; 

  List.iteri (fun i round ->
	Printf.printf "round %d - %d undecided\n" (i + 1) (List.length round);
	List.iteri (fun i choice -> 
	  match choice with
	  | { Tourney.player_pair = Some a, Some b } -> Printf.printf "  %d: %s vs %s\n" i a b
	  | { Tourney.player_pair = None, Some b }-> Printf.printf "  %d: [] vs %s\n" i b
	  | { Tourney.player_pair = Some a, None } -> Printf.printf "  %d: [] vs %s\n" i a
	  | _ -> Printf.printf "%d: [] vs []" i)
	  round)
	undecided 
