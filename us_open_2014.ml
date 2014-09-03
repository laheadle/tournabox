
open Players;;

let tourney = Tourney.init players in
let outcomes = [
  "DJOKOVIC";
  "mathieu";
  "Djokovic";
  "Querrey";
  "Garcia-Lopez";
  "Querrey";

  "Kohlschreiber";
  "Llodra";
  "Kohlschreiber";
  "Struff";
  "Isner";
  "Isner";
  "Tsonga";
  "Nedovyesov";
  "Tsonga";
  "Busta";
  "Paire";
  "Carreno";
  "Verdasco";
  "Kuznetsov";
  "Kuznetsov";
  "Bachinger";
  "Murray";
  "Murray";
  "Wawrinka";
  "Bellucci";
  "Wawrinka";
  "Kavcic";
  "Chardy";
  "Kavcic";
  "Kyrgios";
  "Seppi";
  "Kyrgios";
  "Bolelli";
  "Robredo";
  "Robredo";
  "Nishikori";
  "Andujar";
  "Nishikori";
  "Ebden";
  "Mayer";
  "Mayer";
  "Coric";
  "Estrella Burgos";
  "Estrella Burgos";
  "Gojowczyk";
  "Raonic";
  "Raonic";

"Berdych";
"Klizan";
"Berdych";
"Kudryavtsev";
"Gabashvili";
"Gabashvil";
"Lopez, f";
"Ito, T";
"Lopez, f";
"Thiem";
"Gulbis";
"Thiem";
"Cilic";
"Marchenko";
"Cilic";
"Janowicz";
"Anderson";
"Anderson";
"Simon, G";
"Delbonis";
"Simon, g";
"Tomic";
"Ferrer";
"Ferrer";
"Dimitrov";
"Sela";
"Dimitrov";
"Goffin";
"Sousa";
"Goffin";
"Monfils";
"Gonzalez, A";
"Monfils";
"Lorenzi";
"Gasquet";
"Gasquet";
"Fognini";
"Mannarino";
"Mannarino";
"Smyczek";
"Bautista";
"Bautista";
"Karlovic";
"Granollers";
"Granollers";
"Groth";
"Federer";
"Federer";
"Djokovic";
"Kohlschreiber";
"Djokovic";
"Tsonga";
"Murray";
"Murray";
"Wawrinka";
"Robredo";
"Wawrinka";
"Nishikori";
"Raonic";
"Nishikori";
"Berdych";
"Thiem";
"Berdych";
"Cilic";
"Simon, G";
"Cilic";
"Dimitrov";
"Monfils";
"Monfils";
"Agut";
"Federer";
"Federer"

] in

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false
in

let pick tourney partial =
  let up = String.uppercase in
  let matching = List.filter (fun player ->
	contains (up player) (up partial)) (Tourney.players tourney) in
  if List.length matching = 1 then
	List.hd matching
  else
	raise (Invalid_argument ("bad player " ^ partial))
in

let won tourney partial =
  let player = pick tourney partial in
  Tourney.won tourney player
in

let current_state = List.fold_left (fun tourney win -> won tourney win)
  tourney outcomes
in

(*
let print_tourney t = 
  Printf.printf "%s" (Tourney.to_string t (fun pl -> pl))
in
print_tourney tourney;
*)

let undecided = Tourney.undecided_choices current_state in
let decided = Tourney.decided_choices current_state in

List.iteri (fun i round ->
  Printf.printf "round %d - %d decided\n" (i + 1) (List.length round);
  List.iteri (fun i choice -> 
	match choice with
	| { Tourney.player_pair = Some a, Some b; Tourney.winner = Some c } ->
	  Printf.printf "  %d: %s vs %s - winner: %s\n" i a b c
	| _ -> failwith "bug")
	round)
  decided; 

List.iteri (fun i round ->
  Printf.printf "round %d - %d undecided\n" (i + 1) (List.length round);
  List.iteri (fun i choice -> 
	match choice with
	| { Tourney.player_pair = Some a, Some b } -> Printf.printf "  %d: %s vs %s\n" i a b
	| { Tourney.player_pair = None, Some b }-> Printf.printf "  %d: [] vs %s\n" i b
	| { Tourney.player_pair = Some a, None } -> Printf.printf "  %d: [] vs %s\n" i a
	| _ -> Printf.printf "%d: [] vs []" i)
	round)
  undecided 




