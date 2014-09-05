
type 'player choice = { player_pair: 'player option * 'player option; winner: 'player option }

type ichoice = { iplayer_pair: int option * int option; winner: int option }

type round =  ichoice array
type 'player tourney = { rounds: round array; players: 'player list }
type 'player round_in_progress = 'player choice list

let index_of_player player tourney =
  let rec find i lst =
	match lst with
	  [] -> raise (Invalid_argument "player not found")
	| hd :: tl -> 
	  if hd = player then i
	  else find (i + 1) tl
  in
  find 0 tourney.players

let player_of_index (index: int) (tourney:'player tourney) =
  let rec find i (lst:'player list) =
	match lst with
	  [] -> raise (Invalid_argument "player not found")
	| hd :: tl -> 
	  if i = index then hd
	  else find (i + 1) tl
  in
  find 0 tourney.players

let choice_of_ichoice { iplayer_pair=(p1,p2); winner } (tourney: 'player tourney) =
  let convert (opt: int option) = match opt with
	  None -> None 
	| Some pi -> Some (player_of_index pi tourney)
  in
  { player_pair = (convert p1, convert p2); winner = convert winner }

let num_rounds tourney =
  Array.length tourney.rounds

let init players =
  let len = List.length players in
  Printf.printf "#players: %d\n" len;
  let num_rounds = if Util.power_of_two len then Util.log 2 len
	else raise (Invalid_argument "player length not a power of 2") in
  let empty_ichoice: ichoice = { iplayer_pair=(None,None); winner=None } in
  let init_round i =
	Array.make (Util.pow 2 (num_rounds - i - 1)) empty_ichoice in
  let init_first tourney = 
	let round = tourney.rounds.(0) in
	for i = 0 to Array.length round - 1 do
	  let p1 =  i * 2 in
	  let p2 =  i * 2 + 1 in
	  Util.replace round i (fun ichoice ->
		  { ichoice with iplayer_pair=(Some p1, Some p2) })
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


let playing tourney player =
  let path = path player tourney in
  let nth array n = List.nth (Array.to_list array) n in
  let choices = List.map2 nth (Array.to_list tourney.rounds) path in
  let rec find lst = match lst with
	| [] -> raise (Invalid_argument "no games to win")
	| { iplayer_pair = (Some x, Some y); winner = None } :: tl
		-> if x = player then y else x
	| hd :: tl -> find tl
  in
  find choices

let beat_impl winner loser tourney = 
  let winpath = path winner tourney in
  let losepath = path loser tourney in
  let (playedRound, winI) = path_intersect winpath losepath in
  let nextI = next_position winI in
  Util.replace (tourney.rounds.(playedRound)) winI (fun playedChoice ->
	{ playedChoice with winner = Some winner });
  if playedRound < num_rounds tourney - 1 then
	let schedule nextChoice = match nextChoice with
		{ iplayer_pair = (p1, _p2) ; _ } ->
		  assert (_p2 = None);
		  if p1 = None then
			{ nextChoice with iplayer_pair = (Some winner, None) }
		  else
			{ nextChoice with iplayer_pair = (p1, Some winner) }
	in
	Util.replace (tourney.rounds.(playedRound + 1)) nextI schedule
  else
	();
  tourney

let beat winner loser tourney = 
  let winner = index_of_player winner tourney in
  let loser = index_of_player loser tourney in
  beat_impl winner loser tourney

let won tourney player =
  beat_impl (index_of_player player tourney)
	(playing tourney (index_of_player player tourney)) tourney

let undecided_choices (tourney: 'player tourney) =
  let undecided round =
	let ichoices: ichoice list = List.filter 
	  (function
	  | { iplayer_pair = None, None ; _ } -> false
	  | { winner = Some _ } -> false
	  | _ -> true)
	  (Array.to_list round)
	in
	List.map (fun (ic:ichoice) ->
	  choice_of_ichoice ic tourney)
	  ichoices
  in
  List.map undecided (Array.to_list tourney.rounds)

let decided_choices (tourney: 'player tourney) =
  let decided round =
	let ichoices: ichoice list = List.filter 
	  (function
	  | { winner = Some _ } -> true
	  | _ -> false)
	  (Array.to_list round)
	in
	List.map (fun (ic:ichoice) ->
	  choice_of_ichoice ic tourney)
	  ichoices
  in
  List.map decided (Array.to_list tourney.rounds)

let to_string tourney pfunc =
  List.fold_left (fun str player -> str ^ (pfunc player) ^ "\n")
	"" tourney.players 

let players tourney = tourney.players

let print tourney player_to_string =
  let undecided = undecided_choices tourney in
  let decided = decided_choices tourney in

  List.iteri (fun i round ->
	Printf.printf "round %d - %d decided\n" (i + 1) (List.length round);
	List.iteri (fun i choice -> 
	  match choice with
	  | { player_pair = Some a, Some b; winner = Some c } ->
		Printf.printf "  %d: %s vs %s - winner: %s\n"
		  i (player_to_string a) (player_to_string b) (player_to_string c)
	  | _ -> failwith "bug")
	  round)
	decided; 

  List.iteri (fun i round ->
	Printf.printf "round %d - %d undecided\n" (i + 1) (List.length round);
	List.iteri (fun i choice -> 
	  match choice with
	  | { player_pair = Some a, Some b } ->
		Printf.printf "  %d: %s vs %s\n" i (player_to_string a) (player_to_string b)
	  | { player_pair = None, Some b }->
		Printf.printf "  %d: [] vs %s\n" i (player_to_string b)
	  | { player_pair = Some a, None } -> 
		Printf.printf "  %d: [] vs %s\n" i (player_to_string a)
	  | _ -> Printf.printf "%d: [] vs []" i)
	  round)
	undecided 
