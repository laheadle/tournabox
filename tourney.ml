
type player = int
type choice = { players: player option * player option; winner: player option }

type round =  choice array
type tourney = round array

let replace arr i f =
  let old = arr.(i) in
  arr.(i) <- f(old)


(* integer exponentiation *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a);;

let empty_choice = { players=(None,None); winner=None };;

let init num_rounds =
  let init_round i =
	Array.make (pow 2 (num_rounds - i - 1)) empty_choice in
  let init_first round =
	for i = 0 to Array.length round - 1 do
	  let p1 =  i * 2 in
	  let p2 =  i * 2 + 1 in
	  replace round i (fun choice ->
		  { choice with players=(Some p1, Some p2) })
	done
  in
  let tourney = Array.init num_rounds init_round in
  init_first tourney.(0);
  tourney

let next_position curr = curr / 2;;

let path player tourney =
  let rec iter n np lst =
	if n = 0 then lst
	else
	  let nnp = (next_position np) in
	  iter (n - 1) nnp (nnp :: lst)
  in
  List.rev (iter (Array.length tourney) player [])

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
  replace tourney.(playedRound) winI (fun playedChoice ->
	{ playedChoice with winner = Some winner });
  if playedRound < Array.length tourney - 1 then
	let schedule nextChoice = match nextChoice with
		{ players = (p1, _p2) } ->
		  assert (_p2 = None);
		  if p1 = None then
			{ nextChoice with players = (Some winner, None) }
		  else
			{ nextChoice with players = (p1, Some winner) }
	in
	replace tourney.(playedRound + 1) nextI schedule
;;
