
let play entries outcomes =

  let tourney = Tourney.init entries in

  let current_state = List.fold_left (fun tourney win -> Tennis_tourney.won_str tourney win)
	tourney outcomes
  in

  Tourney.print current_state Tennis_player_entry.to_string
