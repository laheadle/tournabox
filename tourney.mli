
type 'a choice = {
  player_pair : 'a option * 'a option;
  winner : 'a option;
}

type 'a round_in_progress = 'a choice list

type 'a tourney

val init : 'a list -> 'a tourney

val num_rounds: 'a tourney -> int

val undecided_choices: 'a tourney -> 'a round_in_progress list
val decided_choices: 'a tourney -> 'a round_in_progress list

val to_string: 'a tourney -> ('a -> string) -> string

val players: 'a tourney -> 'a list
val num_players: 'a tourney -> int

val won: 'a tourney -> 'a -> 'a tourney

val print: 'a tourney -> ('a -> string) -> unit
val print_by_player: 'a tourney -> ('a -> string) -> unit
