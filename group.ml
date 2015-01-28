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

  val compare_first : contest -> t -> (Entry.slot -> 'a) -> bool
	
  val extract_first_first : t -> (Entry.slot -> 'a) -> 'a

  val sort: (contest -> contest -> int) -> t -> t
end

module Group: GROUP = struct

  type t = contest list
  let make () = []
  let make_one c = [c]
  let iteri = List.iteri
  let length = List.length
  let contains = List.exists
  let add contest group = contest :: group
  let first = function a :: _ -> Some a | _ -> None
  let sort = List.sort

  let compare_length_then_first g1 g2 =
	let cmp = (compare (List.length g1)
				 (List.length g2)) in
	if cmp = 0 then (match g1, g2 with
	  ({ C.entry_pair = a, _ ; winner=aw } :: _,
	   { C.entry_pair = b, _ ; winner=bw } :: _) ->
		let awon = (aw = a) in
		let bwon = (bw = b) in
		if awon = bwon then
		  -(compare a b)
		else
		  compare awon bwon
	| _ -> failwith "bad group compare")
	else
	  cmp

  let compare_first (contest: contest) group f =
	(match contest with
	  { C.entry_pair = (Some a), _ ; _ }
	  -> (match group with
		{ C.entry_pair = (Some b), _ } :: _ ->
		  (f a) = (f b)
	  | _ -> failwith "Bad existing member")
	| _ -> failwith "Bad contest for group")

  let extract_first_first (lst: t) f =
	match lst with
	  contest :: tl -> 
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
	mutable lst: Group.t list;
	spec: grouping_spec;
  }
  let make spec = {
	lst = [];
	spec = spec;
  }

  let add_contest contest groups =
	let rec add_contest_iter contest lst =
	  match lst with [] -> [Group.make_one contest]
	  | hd :: tl -> 
		let group_result = groups.spec#in_group contest hd in
		if group_result.Ttypes.quit then lst
		else
		  if group_result.Ttypes.this_group then (Group.add contest hd) :: tl
		  else
			hd :: (add_contest_iter contest tl)
	in
	groups.lst <- add_contest_iter contest groups.lst

  let sort t =
	t.lst <- List.sort
	  t.spec#compare_group
	  (List.map (Group.sort t.spec#compare_contest) t.lst);
	t

  let iteri f (groups: t) = List.iteri f groups.lst
  let length (groups: t) = List.length groups.lst

end


(*


*)


