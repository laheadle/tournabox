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
