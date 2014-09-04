

let pick tourney partial =
  Util.pick (Tourney.players tourney) partial

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
