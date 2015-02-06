
let is_some = function None -> false | _ -> true
let is_none = function None -> true | _ -> false

let get_option option =
  assert (is_some option);
  match option with
	None -> assert false
  | Some x -> x

let map_option f =
  function None -> None
  | Some x -> Some (f x)

(* integer exponentiation *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a);;


let replace arr i f =
  let old = arr.(i) in
  arr.(i) <- f(old)


let power_of_two n  =
  let rec iter k =
	if k = n then true
	else if k > n then false
	else iter (k * 2)
  in
  if n < 1 then false
	else
	iter 1

let log base n =
  let rec iter k j =
	if j > n then
	  raise (Invalid_argument "bad log")
	else
	  if j = n then
		k
	  else
		iter (k + 1) (j * base)
  in
  iter 1 base

let contains ~within s2 =
  (* Steps backward from the end *)
  let rec is_at pos i =
	if not (within.[pos + i] = s2.[i]) then false
	else if i = 0 then true
	else is_at pos (i -1)
  in
  let rec try_all pos =
	if is_at pos (String.length s2 - 1) then
	  true
	else if pos = 0 then false
	else try_all (pos - 1)
  in
  let len2 = String.length s2 in
  let len1 = String.length within in
  let start = len1 - len2 in
  if len2 = 0 then true
  else
	if start < 0 || len1 = 0 then false
	else
	  try_all start

let strip_spaces str =
  let spaces = Regexp.regexp "^\\s*(.*?)\\s*$" in
  let matchs = Regexp.search spaces str 0 in
  match matchs with
	None -> str
  | Some (i, result) ->
	match Regexp.matched_group result 1 with
	  Some str -> str | None -> assert false


let hd_exn = function [] -> assert false | a :: b -> a


let pick list partial to_string =
  let up = String.uppercase in
  let matching = List.filter (fun thing ->
	contains ~within: (up (to_string thing)) (up partial)) list in
  if List.length matching = 1 then
	List.hd matching
  else
	(* In case of multiple matches, if one of the candidates matches
	   exactly, then pick it *)
	let exactly_matching = List.filter (fun thing ->
	  up (to_string thing) = (up partial)) matching in
	if List.length exactly_matching = 1 then
	  List.hd exactly_matching
	else
	  failwith
		(List.fold_left 
		   (fun strs thing ->
			 strs ^ (to_string thing) ^ "; ")
		   ("Error: Invalid winner: \n'" ^ partial ^ "'\nmatches:\n")
		   matching)

let id x = x

let filter_then_map ~mapf ~filterf lst =
  List.map mapf (List.filter filterf lst)
