
let pick tourney partial =
  Util.pick (Tourney.players tourney) partial (fun entry -> entry.Player_entry.player.Player.name)

let won_str tourney partial =
  let player = pick tourney partial in
  Tourney.won tourney player

