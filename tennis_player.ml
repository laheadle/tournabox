module C = Choice

module M: Player.S =
  struct
	type t = { pname: string; country: string }

	let of_string str =
	  let len = String.length str in
	  let country = String.sub str (len - 3) 3 in
	  let name = String.sub str 0 (len - 3) in
	  { pname = name; country = country }

	let to_string x = x.pname ^ " " ^ x.country

  type group_result =  {
	quit: bool;
	this_group: bool;
  }
  type column = string * string option

  type grouping_spec = {
	name:string;
	header_name: num_rounds:int -> pos:int -> t Choice.t list -> string;
	compare_choice: t Choice.t -> t Choice.t -> int;
	compare_group: t Choice.t list -> t Choice.t list -> int;
	in_group: t Choice.t -> t Choice.t list -> group_result;
	column_extractor: int -> int -> t Choice.t -> column list;
  }

	let grouping_specs = [
	  {
		header_name = (fun ~num_rounds ~pos lst ->
		  match lst with
			choice :: tl -> 
			  (match choice with
				{ C.entry_pair = (Some a), _ ; _ }
				-> a.country
			  | _ -> failwith "Bad entry for header")
		  | _ -> failwith "Bad group for header");
		name = "By Country";
		compare_choice = (fun c1 c2 ->
		  compare (C.first c1).pname (C.first c2).pname);
		compare_group = 
		  (fun g1 g2 ->
			let cmp = -(compare (List.length g1)
						  (List.length g2)) in
			if cmp = 0 then (match g1, g2 with
			  ({ C.entry_pair = (Some a), _ ; _ } :: _,
			   { C.entry_pair = (Some b), _ ; _ } :: _) ->
				compare a b
			| _ -> failwith "bad group compare")
			else
			  cmp);
		in_group =
		  (fun choice group -> {
			quit = false;
			this_group =
			  (match choice with
				{ C.entry_pair = (Some a), _ ; _ }
				-> (match group with
				  { C.entry_pair = (Some b), _ } :: _ ->
					a.country = b.country
				| _ -> failwith "Bad existing member")
			  | _ -> failwith "Bad choice for group");
		  });
		column_extractor =
		(fun num pos choice ->
		  match choice with
		  | { C.entry_pair = Some a, Some b; winner = Some c } ->
			let outcome = if c = a then "Defeated" else "Was defeated by" in
			[ (to_string a), None;
			  outcome,
			  (Some (if c = a then "tourney-won" else "tourney-lost"));
			  (to_string b), None]
		  | { C.entry_pair = Some a, Some b; winner = None } ->
			[  (to_string a), None;
			  "will play", None;
			  (to_string b), None ]
		  | { C.entry_pair = Some a, None; winner = None } ->
			[]
		  | _ -> failwith "bug");
	  }
	]


  end

