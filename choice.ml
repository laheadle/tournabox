type 'a t = {
  round: int;
  position: int;
  entry_pair : 'a option * 'a option;
  winner : 'a option;
}

let first = function
  | { entry_pair = (Some a), _ ; _ } -> a
  | _ -> failwith "First"


let map f = function
{ entry_pair=(p1,p2); winner; _ } as choice ->
  let convert opt = match opt with
	  None -> None 
	| Some pi -> Some (f pi)
  in
  { choice with
	entry_pair = (convert p1, convert p2);
	winner = convert winner;
  }

let compare_length_then_first g1 g2 =
  let cmp = (compare (List.length g1)
				(List.length g2)) in
  if cmp = 0 then (match g1, g2 with
	({ entry_pair = a, _ ; winner=aw } :: _,
	 { entry_pair = b, _ ; winner=bw } :: _) ->
	  let awon = (aw = a) in
	  let bwon = (bw = b) in
	  if awon = bwon then
		compare a b
	  else
		compare awon bwon
  | _ -> failwith "bad group compare")
  else
	cmp

let compare_first choice group f =
  (match choice with
	{ entry_pair = (Some a), _ ; _ }
	-> (match group with
	  { entry_pair = (Some b), _ } :: _ ->
		(f a) = (f b)
	| _ -> failwith "Bad existing member")
  | _ -> failwith "Bad choice for group")

let extract_first_first lst f =
	match lst with
	  choice :: tl -> 
		f (first choice)
	| _ -> failwith "Bad extract_first_first"

