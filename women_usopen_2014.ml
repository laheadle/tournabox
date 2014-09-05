
open Events;;

let db = Player.make_db () in

let tourney = Tourney.init (Events.women_usopen_2014 db) in

let current_state = List.fold_left (fun tourney win -> Seeded_tourney.won_str tourney win)
  tourney Events.outcomes_women_usopen_2014
in

Tourney.print current_state Player_entry.to_string
