module C = Contest

type round =  int Contest.t array
type tourney = { rounds: round array;
				 num_slots: int;
				 byes: (int, bool) Hashtbl.t;
				 entries_list: Entry.t list;
				 entries: (int, Entry.t) Hashtbl.t;
				 indices: (Entry.t, int) Hashtbl.t
			   }

let index_of_entry entry tourney =
  Hashtbl.find tourney.indices entry

let entry_of_index index tourney =
  Hashtbl.find tourney.entries index

let contest_of_icontest icontest tourney =
  C.map (fun i ->
	try
	  ignore(Hashtbl.find tourney.byes i);
	  Entry.Bye
	with _ ->
	  Entry.Somebody (entry_of_index i tourney)) icontest

let log_2 len =
  if Util.power_of_two len then Util.log 2 len
  else failwith ("The number of entries must be a power of two")
	
let num_rounds tourney =
  let len = Array.length tourney.rounds.(0) in
  log_2 len + 1

let entries tourney = tourney.entries
let entries_list tourney = tourney.entries_list

let next_position curr = curr / 2;;

let path entry tourney =
  let rec iter n np lst =
	if n = 0 then lst
	else
	  let nnp = (next_position np) in
	  iter (n - 1) nnp (nnp :: lst)
  in
  List.rev (iter (Array.length tourney.rounds) entry [])

let path_intersect winpath losepath =
  let rec iter i lst1 lst2 =
	match lst1, lst2 with
	  [],_ 	| _,[] -> failwith ("BUG: no intersection of paths")
	| p1::tail1, p2::tail2 ->
	  if (p1 = p2) then
		  (* (playedRound, winI) *)
		i, p1
	  else
		(iter (i+1) tail1 tail2)
  in iter 0 winpath losepath 

let index_to_string tourney index =
  let open Entry in
  to_string (entry_of_index index tourney)

let playing tourney index =
  let path = path index tourney in
  let nth array n = array.(n) in
  let contests = List.map2 nth (Array.to_list tourney.rounds) path in
  let rec find lst = match lst with
	| [] -> failwith ("Error: Invalid Winner:" ^ (index_to_string tourney index) ^
						 ". \n\nThis player has already lost.")
	| { C.entry_pair = (Some x, Some y); winner = None } :: tl
	  -> if x = index then y else x
	| { C.entry_pair = (Some x, None); _ } :: _
	| { C.entry_pair = (None, Some x); _ } :: _ when x = index
	  -> failwith ("Error: Invalid Winner:" ^ (index_to_string tourney index) ^
						 ". \n\nThis player's next opponent is not yet determined.")
	| hd :: tl -> find tl
  in
  find contests

let is_true hash key =
  try
	Hashtbl.find hash key
  with
	_ -> false

let is_bye tourney i = is_true tourney.byes i

let rec won tourney index =
  let impl winner loser = 
	let winpath = path winner tourney in
	let losepath = path loser tourney in
	let (playedRound, winI) = path_intersect winpath losepath in
	let nextI = next_position winI in
	Util.replace (tourney.rounds.(playedRound)) winI (fun playedContest ->
	  { playedContest with C.winner = Some winner });
	let schedule () =
	  let next_round = tourney.rounds.(playedRound + 1) in
	  let next_contest = next_round.(nextI) in
	  let will_have_bye, to_play =
		match next_contest with
		  { C.entry_pair = (p1, _p2) ; _ } ->
			assert (_p2 = None);
			match p1 with
			  None ->
				false,
				{ next_contest with C.position = nextI;
				  C.entry_pair = (Some winner, None) }
			  | Some i_p1 ->
				is_bye tourney i_p1,
				{ next_contest with C.position = nextI;
				  C.entry_pair = (p1, Some winner) }
	  in
	  next_round.(nextI) <- to_play;
	  (* Go ahead and advance the bye *)
	  if will_have_bye then won tourney winner else tourney
	in
	if playedRound < num_rounds tourney - 1 then
	  schedule ()
	else
	  tourney
  in
  Tlog.infof ~section:Tlog.playing "won %d %d\n" index (playing tourney index);
  impl index (playing tourney index)


(* The first round is initialized with full icontests; other rounds are
   initialized with empty icontests *)
let init entries_list =
  let open Entry in
  let len = List.length entries_list in
  let num_rounds = log_2 len in
  Tlog.noticef ~section:Tlog.input "%d rounds\n" num_rounds;
  let empty_icontest ~(round: int) = {
	C.entry_pair=(None,None);
	winner=None;
	round;
	position = 0;
  } in
  let init_round i =
	Array.make (Util.pow 2 (num_rounds - i - 1))
	  (empty_icontest ~round:i) in
  let byes = Hashtbl.create 4 in
  let init_first round =
	for i = 0 to Array.length round - 1 do
	  let p1 =  i * 2 in
	  let p2 =  i * 2 + 1 in
	  Util.replace round i (fun icontest ->
		{ icontest with C.entry_pair=(Some p1, Some p2);
		  position = p1 })
	done
  in
  let entries = Hashtbl.create 200 in
  let indices = Hashtbl.create 200 in
  let rec fill_hashes n lst =
	match lst with
	  [] -> ()
	| hd :: tl ->
	  if Entry.is_bye hd then
		Hashtbl.add byes n true
	  else begin
		let t = fetch hd in
		Hashtbl.add entries n t;
		Hashtbl.add indices t n;
	  end;
	  fill_hashes (n + 1) tl
  in
  fill_hashes 0 entries_list;
  let rounds = Array.init num_rounds init_round in
  let first =  rounds.(0) in
  init_first first;
  let rec perform_byes tourney round_index contest_index =
	Tlog.debugf ~section:Tlog.playing "byes round %d contest %d" round_index contest_index;
	let do_round round =
	  if contest_index < Array.length round then
		let tourney =
		  match round.(contest_index) with
			{ C.entry_pair = Some a, Some b; winner=None } ->
			  if is_true byes a then
				(won tourney b)
			  else if is_true byes b then
				(won tourney a)
			  else
				tourney
		  | _ -> tourney
		in
		perform_byes tourney round_index (contest_index + 1)
	  else
		perform_byes tourney (round_index + 1) 0
	in
	if round_index < Array.length tourney.rounds then
	  do_round rounds.(round_index)
	else
	  tourney
  in
  let tourney = {
	entries_list = Util.filter_then_map ~mapf: fetch
	  ~filterf: is_t
	  entries_list;
	byes;
	entries;
	indices;
	rounds;
	num_slots = Array.length first;
  } in
  perform_byes tourney 0 0

let won_str tourney partial =
  Tlog.debugf ~section:Tlog.playing "won_str %s\n" partial;
  let entry =
	Util.pick (entries_list tourney) partial Entry.to_string
  in
  let i = (index_of_entry entry tourney) in
  won tourney i

let num_slots tourney = tourney.num_slots
