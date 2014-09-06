
let play event outcomes =
  let db = Tennis_player.make_db () in

  let tourney = Tourney.init (event db) in

  let current_state = List.fold_left (fun tourney win -> Tennis_tourney.won_str tourney win)
	tourney outcomes
  in

  Tourney.print current_state Tennis_player_entry.to_string
