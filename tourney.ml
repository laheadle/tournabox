
type 'player choice = { player_pair: 'player option * 'player option; winner: 'player option }

type 'player round =  'player choice array
type 'player tourney = { rounds: 'player round array; players: 'player list }
type 'player round_in_progress = 'player choice list

let empty_choice = { player_pair=(None,None); winner=None };;

let index_of_player player tourney =
  let rec find i lst =
	match lst with
	  [] -> raise (Invalid_argument "player not found")
	| hd :: tl -> 
	  if hd = player then i
	  else find (i + 1) tl
  in
  find 0 tourney.players

let num_rounds tourney =
  Array.length tourney.rounds

let init (players: 'player list) =
  let num_rounds = List.length players in
  let init_round i =
	Array.make (Util.pow 2 (num_rounds - i - 1)) empty_choice in
  let init_first tourney = 
	let round = tourney.rounds.(0) in
	for i = 0 to Array.length round - 1 do
	  let p1 =  i * 2 in
	  let p2 =  i * 2 + 1 in
	  Util.replace round i (fun choice ->
		  { choice with player_pair=(Some p1, Some p2) })
	done
  in
  let tourney = { 
	players = players;
	rounds = Array.init num_rounds init_round } in
  init_first tourney;
  tourney

let next_position curr = curr / 2;;

let path player tourney =
  let rec iter n np lst =
	if n = 0 then lst
	else
	  let nnp = (next_position np) in
	  iter (n - 1) nnp (nnp :: lst)
  in
  List.rev (iter (Array.length tourney.rounds) player [])

let path_intersect winpath losepath =
  let rec iter i lst1 lst2 =
	match lst1, lst2 with
	  [],_ 	| _,[] -> raise (Invalid_argument "bad lengths")
	| p1::tail1, p2::tail2 ->
	  if (p1 = p2) then
		(* (playedRound, winI) *)
		i, p1
	  else
		(iter (i+1) tail1 tail2)
  in iter 0 winpath losepath 


let beat winner loser tourney = 
  let winpath = path winner tourney in
  let losepath = path loser tourney in
  let (playedRound, winI) = path_intersect winpath losepath in
  let nextI = next_position winI in
  Util.replace (tourney.rounds.(playedRound)) winI (fun playedChoice ->
	{ playedChoice with winner = Some winner });
  if playedRound < num_rounds tourney - 1 then
	let schedule nextChoice = match nextChoice with
		{ player_pair = (p1, _p2) ; _ } ->
		  assert (_p2 = None);
		  if p1 = None then
			{ nextChoice with player_pair = (Some winner, None) }
		  else
			{ nextChoice with player_pair = (p1, Some winner) }
	in
	Util.replace (tourney.rounds.(playedRound + 1)) nextI schedule


let undecided_choices tourney =
  let undecided round =
	List.filter 
	  (function
	  | { player_pair = None, None ; _ } -> false
	  | _ -> true)
	  (Array.to_list round)
  in
  List.map undecided (Array.to_list tourney.rounds)
