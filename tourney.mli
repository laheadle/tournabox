
type 'player choice = {
  players : 'player option * 'player option;
  winner : 'player option;
}

type 'player tourney

val init : 'player list -> 'player tourney

val beat : 'player -> 'player -> 'player tourney -> 'player tourney

val progress : 'player -> 'player tourney -> 'player choice list

val num_rounds: 'player_tourney -> int

val round_scheduled: 'player -> 'player -> 'player_tourney -> int
