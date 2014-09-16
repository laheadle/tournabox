
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

let won tourney player =
  let impl winner loser = 
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
  in
  impl (index_of_player player tourney)
	(playing tourney (index_of_player player tourney))

let filter_choices tourney (fn: ichoice -> bool) =
  let filter round =
	let ichoices = List.filter 
	  fn
	  (Array.to_list round)
	in
	List.map (fun ic ->
	  choice_of_ichoice ic tourney)
	  ichoices
  in
  List.map filter (Array.to_list tourney.rounds)

let filter_choices_paired tourney fn1 fn2 =
  let mapper round =
	let filter fn =
	  List.filter 
		fn
		(Array.to_list round) in
	let tochoices ichoices =
	  List.map
		(fun ic -> choice_of_ichoice ic tourney)
		ichoices
	in
	let ichoices1 = filter fn1 in
	let ichoices2 = filter fn2 in
	(tochoices ichoices1, tochoices ichoices2)
  in
  List.map mapper (Array.to_list tourney.rounds)

let nonempty_choices tourney =
  let undecided =
	function { iplayer_pair = None, None ; _ } -> false
	| { winner = Some _ } -> false
	| _ -> true
  in
  let decided =
	function { winner = Some _ } -> true
	| _ -> false
	in
  filter_choices_paired tourney undecided decided

let undecided_choices tourney =
  filter_choices tourney
	(function { iplayer_pair = None, None ; _ } -> false
	| { winner = Some _ } -> false
	| _ -> true)

let decided_choices tourney =
  filter_choices tourney
	(function { winner = Some _ } -> true
	| _ -> false)

let num_players tourney = List.length tourney.players

let choices_per_player tourney ~compare_player =
  let choices = Array.make (num_players tourney) [] in
  let replace k ic =
	Util.replace choices k (fun lst ->
	  (choice_of_ichoice ic tourney)  :: lst) in
  for i = 0 to Array.length tourney.rounds - 1 do
	let round = tourney.rounds.(i) in
	for i = 0 to Array.length round - 1 do
	  match round.(i) with
		{ iplayer_pair = (Some k, Some j) } as ichoice ->
		  replace k ichoice;
		  (* Make sure the reference player comes first *)
		  replace j { ichoice with iplayer_pair = ( Some j, Some k ) };
	  | { iplayer_pair = (None, Some k) } as ichoice ->
		  replace k { ichoice with iplayer_pair = ( Some k, None ) };
	  | { iplayer_pair = (Some k, None) } as ichoice ->
		  replace k ichoice;
	  | _ -> (); (* skip empties *)
	done
  done;
  Array.sort compare_player choices;
  choices

let to_string tourney pfunc =
  List.fold_left (fun str player -> str ^ (pfunc player) ^ "\n")
	"" tourney.players 

let players tourney = tourney.players

let delete_children node =
  let children = node##childNodes in
  for i = 0 to children##length - 1 do
    Js.Opt.iter (node##firstChild) (fun child -> Dom.removeChild node child) ;
  done

let doc = Dom_html.document

let print_by_player tourney player_to_string container =
  let by_player = choices_per_player tourney 
	~compare_player: (fun lst1 lst2 ->
	  compare (List.length lst2) (List.length lst1)) in

  let do_player_choices i choices =
	let player_str =
 	  (match List.hd choices with { player_pair = (Some a, _) } ->
		player_to_string a | _ -> failwith "bug") in
	let len = List.length choices in
	let table = Jsutil.table (Some "tourney-outerTable") in
	let header = Dom_html.createTr doc in
	Jsutil.addTd header player_str (Some "tourney-header");
	Dom.appendChild table header;
	let do_choice i choice =
	  let () = match choice with
		| { player_pair = Some a, Some b; winner = Some c } ->
		  let outcome = if c = a then "Defeated" else "Was defeated by" in
		  let row = Dom_html.createTr doc in	
		  Jsutil.addTd row outcome
			(Some (if c = a then "tourney-won" else "tourney-lost"));
		  Jsutil.addTd row (player_to_string b) None;
		  Jsutil.addTd row ("In round " ^ (string_of_int (len - i))) None;
		  Dom.appendChild table row
		| { player_pair = Some a, Some b; winner = None } ->
		  let row = Dom_html.createTr doc in	
		  Jsutil.addTd row "will play" None;
		  Jsutil.addTd row (player_to_string b) None;
		| { player_pair = Some a, None; winner = None } ->
		  let row = Dom_html.createTr doc in	
		  Jsutil.addTd row "will play" None;
		  Jsutil.addTd row "To be determined" None;
		| _ -> failwith "bug" in
	  Dom.appendChild container table;
	  ();
	in
	List.iteri do_choice choices
  in
  Array.iteri do_player_choices by_player

let print_by_round tourney player_to_string outer_container =
  let nonempty = List.rev (nonempty_choices tourney) in
  let num_rounds = List.length nonempty in

  let container = Jsutil.table None in
  Dom.appendChild outer_container container;

  let iter i (undecided, decided) =
	let header = Dom_html.createTr doc in
	Jsutil.addTd header
	  (if i = 0 then
		  "Finals\n"
	   else
		  (if i = 1 then
			  "Semifinals"
		   else
			  (if i = 2 then
				  "Quarterfinals"
			   else
				  Printf.sprintf
					"Round %d (%d matches)"
					(num_rounds - i)
					(List.length undecided + List.length decided))))
	  (Some "tourney-header");
	Dom.appendChild container header;

	let riter i choice = 
	  let row = Dom_html.createTr doc in
	  (match choice with
	  | { player_pair = Some a, Some b; winner = Some c } ->
		let loser = if c = a then b else a in
		Jsutil.addTd row (player_to_string c) None;
		Jsutil.addTd row "defeated" (Some "tourney-won");
		Jsutil.addTd row (player_to_string loser) None;
	  | { player_pair = Some a, Some b; winner = None } ->
		Jsutil.addTd row (player_to_string a) None;
		Jsutil.addTd row "will face" (Some "tourney-willFace");
		Jsutil.addTd row (player_to_string b) None;
	  | { player_pair = Some a, None; _ } ->
		Jsutil.addTd row (player_to_string a) None;
		Jsutil.addTd row "will face" (Some "tourney-willFace");
		Jsutil.addTd row "(To be decided)" None;
	  | _ ->
		failwith "bug 4");
	  Dom.appendChild container row in
	let emptyRow = Dom_html.createTr doc in
	Jsutil.addTd emptyRow " " None;
	List.iteri riter undecided;
	List.iteri riter decided;
	Dom.appendChild container emptyRow
  in
  List.iteri iter nonempty


(*
  List.iteri (fun i round ->
	Printf.printf "round %d - %d undecided\n" (i + 1) (List.length round);
	List.iteri (fun i choice -> 
	  match choice with
	  | { player_pair = Some a, Some b } ->
		Printf.printf "  %d: %s vs %s\n" (i + 1) (player_to_string a) (player_to_string b)
	  | { player_pair = None, Some b }->
		Printf.printf "  %d: [] vs %s\n" (i + 1) (player_to_string b)
	  | { player_pair = Some a, None } -> 
		Printf.printf "  %d: [] vs %s\n" (i + 1) (player_to_string a)
	  | _ -> Printf.printf "%d: [] vs []" (i + 1))
	  round)
	undecided 

*)

let show tourney player_to_string =
  let container = Jsutil.getElementById_exn "container"
  in
  let checkGroupByRounds = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
  let checkGroupByPlayers = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
  let inner = Dom_html.createDiv doc in
  let top = Dom_html.createDiv doc in
  let add elt = Dom.appendChild container elt in
  let addTop elt = Dom.appendChild top elt in
  top##className <- (Js.string "tourney-menuDiv");
  add top;
  Jsutil.textNode "By Round" |> addTop;
  addTop checkGroupByRounds;
  Jsutil.textNode "By Player" |> addTop;
  addTop checkGroupByPlayers;
  add inner;
  print_by_round tourney player_to_string inner;
  checkGroupByRounds##checked <- Js._true;
  let clicks positive negative print =
	(* toggle radio buttons *)
	Lwt_js_events.clicks positive (fun event event_loop ->
	delete_children inner;
	negative##checked <- Js._false;
	Lwt.return (print tourney player_to_string inner))
  in
  ignore (clicks checkGroupByRounds checkGroupByPlayers print_by_round);
  ignore (clicks checkGroupByPlayers checkGroupByRounds print_by_player)


