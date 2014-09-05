
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

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let pick list partial to_string =
  let up = String.uppercase in
  let matching = List.filter (fun thing ->
	contains (up (to_string thing)) (up partial)) list in
  if List.length matching = 1 then
	List.hd matching
  else
	raise (Invalid_argument
			 (List.fold_left 
				(fun strs thing ->
				  strs ^ (to_string thing) ^ "; ")
				("bad thing: " ^ partial ^ " could be: ")
				matching))
