module C = Contest

type contest = Entry.slot C.t

module type GROUP = sig
  type t
  val make: unit -> t
  val make_one: contest -> t
  val iteri: (int -> contest -> unit) -> t -> unit
  val length: t -> int
  val contains: (contest -> bool) -> t -> bool
  val first: t -> contest option
  val add: contest -> t -> t
  (** This is for sorting groups. First, compare their lengths. Then
	  look at the first player in each first contest. If they both won
	  or both lost, compare the contestants. Otherwise compare the
	  win/loss results. *)
  val compare_length_then_first : t ->  t -> int

  val match_first : contest -> t -> (Entry.slot -> 'a) -> bool
	
  val extract_first_first : t -> (Entry.slot -> 'a) -> 'a

  val sort: (contest -> contest -> int) -> t -> t
end

module Group: GROUP = struct

  type t = contest Js.js_array Js.t
  let make () : t = jsnew Js.array_empty ()
  let iteri f (g: t) = g##forEach (fun contest i _ ->
	f i contest)
  let length (g: t) = g##length
  let contains f (g: t) = g##some (fun contest _ _ ->
	f contest |> Js.bool) |> Js.to_bool
  let add contest (g: t) = ignore(g##push(contest)); g
  let first (g: t) = Js.array_get g 0 |> Js.Optdef.to_option
  let sort f (g: t) =
	g##sort (Js.wrap_callback
			   (fun x y ->
				 (float_of_int (f x y))))
  let make_one (c: contest) = add c (make ())

  let compare_length_then_first (g1: t) (g2: t) =
	let cmp = (compare (g1##length)
				 (g2##length)) in
	if cmp = 0 then (match (first g1), (first g2) with
	  (Some { C.entry_pair = a, _ ; winner=aw },
	   Some { C.entry_pair = b, _ ; winner=bw }) ->
		let awon = (aw = a) in
		let bwon = (bw = b) in
		if awon = bwon then
		  -(compare a b)
		else
		  compare awon bwon
	| _ -> failwith "bad group compare")
	else
	  cmp

  let match_first (contest: contest) (group: t) f =
	contest |> C.first |> f
	=
	(group |> first |> Util.get_option |> C.first |> f)

  let extract_first_first (g: t) f =
	match (first g) with
	  Some contest ->
		f (C.first contest)
	| _ -> failwith "Bad extract_first_first"

end

class type grouping_spec = object
  method name:string
  method header_spec: num_rounds:int -> num_groups:int -> pos:int -> Group.t -> Ttypes.header_spec
  method compare_contest: contest -> contest -> int
  method compare_group: Group.t -> Group.t -> int
  method in_group: contest -> Group.t -> Ttypes.group_result
  method column_extractor: int -> int -> contest -> Ttypes.column list
end

module type GROUP_LIST = sig
  type t
  val make: grouping_spec -> t 
  val add_contest: contest -> t -> unit
  val sort: t -> t
  val iteri: (int -> Group.t -> unit) -> t -> unit
  val length: t -> int
end

module GroupList: GROUP_LIST = struct
  type t = {
	mutable raw: Group.t Js.js_array Js.t;
	spec: grouping_spec;
  }
  let make spec = {
	raw = jsnew Js.array_empty ();
	spec = spec;
  }

  let add_contest contest groups =
	let raw = groups.raw in
	let len = raw##length in
	let push_new_group () = 
	  ignore(raw##push(Group.make_one contest));
	in
	let rec add_contest_iter rawI =
	  if rawI = len then push_new_group ()
	  else
		let group =
		  Js.Optdef.get (Js.array_get raw rawI)
			(fun () -> assert false)
		in
		let group_result = groups.spec#in_group contest group in
		if group_result.Ttypes.quit then ()
		else
		  if group_result.Ttypes.this_group then
			ignore(Group.add contest group)
		  else
			add_contest_iter (rawI + 1)
	in
	add_contest_iter 0

  let sort t =
	t.raw <- (t.raw##map
				(fun g _ _ ->
				  Group.sort t.spec#compare_contest g));
	ignore(t.raw##sort
			 (Js.wrap_callback
				(fun x y ->
				  (float_of_int (t.spec#compare_group x y)))));
	t

  let iteri f (groups: t) = groups.raw##forEach (fun group i _ ->
	f i group)
  let length (groups: t) = groups.raw##length

end


(*


*)


