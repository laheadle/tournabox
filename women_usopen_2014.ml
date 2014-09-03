
open Events;;

let tourney = Tourney.init women_usopen_2014 in

let current_state = List.fold_left (fun tourney win -> String_tourney.won tourney win)
  tourney outcomes_women_usopen_2014
in
String_tourney.print_tourney current_state

(*
let print_tourney t = 
  Printf.printf "%s" (Tourney.to_string t (fun pl -> pl))
in
print_tourney tourney;
*)





