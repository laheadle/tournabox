type contest = Entry.slot Contest.t

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

  (** This is for determining whether a contest belongs in a group at
	  all. It checks the first contestant of the contest against the
	  first contestant of the first contest in the group. The function
	  argument is applied to both contestants, and the results are
	  tested with '='. *)
  val match_first : contest -> t -> (Entry.slot -> 'a) -> bool

  (** 
	  This is for extracting Group headers. For example, the header
	  of a group in the `By Country' Grouping is the name of the
	  country.

	  [extract_first_first group f] calls f on the first (slot)
	  contestant of the first contest in group, and returns what f
	  returns.  *)
  val extract_first_first : t -> (Entry.slot -> 'a) -> 'a

  (** Sort a group with a sorting function *)
  val sort: (contest -> contest -> int) -> t -> t
end

module Group: GROUP

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

module GroupList: GROUP_LIST

