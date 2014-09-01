
type 'player choice = {
  player_pair : 'player option * 'player option;
  winner : 'player option;
}

type 'player round_in_progress = 'player choice list

type 'player tourney

val init : 'player list -> 'player tourney

val beat : 'player -> 'player -> 'player tourney -> 'player tourney

val num_rounds: 'player tourney -> int

(*
val round_scheduled: 'player -> 'player -> 'player_tourney -> int

val progress : 'player -> 'player tourney -> 'player choice list
  *)

val undecided_choices: 'player tourney -> 'player round_in_progress list
