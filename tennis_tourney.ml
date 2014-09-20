
let pick tourney partial =
  Util.pick (Tourney.entries tourney) partial (fun entry ->
	entry.Tennis_player_entry.player.Tennis_player.name)

let won_str tourney partial =
  let entry = pick tourney partial in
  Tourney.won tourney entry

