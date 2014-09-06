
let pick tourney partial =
  Util.pick (Tourney.players tourney) partial (fun entry ->
	entry.Tennis_player_entry.player.Tennis_player.name)

let won_str tourney partial =
  let player = pick tourney partial in
  Tourney.won tourney player

