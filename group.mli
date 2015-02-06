(** Types and functions for grouping, sorting, and rendering
    	contests. *)

(** An alias for the type of contests that are grouped. *)
type contest = Entry.slot Contest.t

(** This module type encapsulates types and functions for working with
    	groups of contests *)
module type GROUP = sig

  (** The basic grouping unit of Tournabox. An ordered collection of
      	  contests *)
  type t

  (** Create a new group *)
  val make: unit -> t

  (** Create a group containing one contest *)
  val make_one: contest -> t

  (** [iteri f g] runs f on each contest in g *)
  val iteri: (int -> contest -> unit) -> t -> unit

  (** Get the length of a group *)
  val length: t -> int

  (** [contains f g] yields true if g contains a contest matching f *)
  val contains: (contest -> bool) -> t -> bool

  (** Get the first contest of a group, if there is one. *)
  val first: t -> contest option

  (** Add a contest to a group. *)
  val add: contest -> t -> unit

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
  val sort: (contest -> contest -> int) -> t -> unit
end

module Group: GROUP

(** Return value of {! Group.grouping_spec.in_group} *)
type group_result =  {
  quit: bool;
  this_group: bool;
}

(** A way of grouping and presenting an ordered collection of groups
    	of contests.
    	{b This is one of the central abstractions of Tournabox.} *)
class type grouping_spec = object

  (** The name of the grouping_spec. A legal value of the
      	  comma-separated tournabox-groups container attribue. *)
  method name:string

  (** Returns a {! Columns.header_spec} to specify how to display the header of a
      	  group. Most of the parameters are only used by `By Round' to
      	  calculate the round number displayed.

      	  Given the total number of rounds in the tournabox upon
      	  completion; the number of groups in the {! Group.GroupList}
      	  (i.e. the number of rounds played so far), the index of the
      	  group in the {! Group.GroupList} (i.e. the round); and
      	  the group in need of a header.

      	  All of the grouping_specs except {! Grouping_specs.round} simply look at the
      	  first contest in the group, and pull out a prominent field like
      	  name or country.  *)
  method header_spec: num_rounds:int -> num_groups:int -> pos:int
    -> Group.t -> Columns.header_spec

  (** Compares two contests for the purposes of sorting them within a
      	  group. *)
  method compare_contest: contest -> contest -> int

  (** Compares two groups for the purposes of sorting them within a
      	  GroupList. *)
  method compare_group: Group.t -> Group.t -> int

  (** Returns whether a contest is in a group. If the return value has
      	  {! group_result.quit} = true, then give up: stop trying
      	  to find a group to put the contest in. *)
  method in_group: contest -> Group.t -> group_result

  (** Returns the columns to render for a contest. Given the number
      	  of contests in the group; the index of the contest in the
      	  group; and the contest. *)
  method extract_columns: num_contests:int -> index:int
    -> contest -> Columns.column list
end


(** This module type specifies types and functions for working with
    	ordered collections of groups of contests. Tournabox displays one
    	of them at any given time *)

module type GROUP_LIST = sig

  (** An ordered collection of groups of contests. *)
  type t

  (** Creation *)
  val make: grouping_spec -> t 

  (** Add a contest to a collection *)
  val add_contest: contest -> t -> unit

  (** Sort it. *)
  val sort: t -> unit

  (** Iterate over the groups. *)
  val iteri: (int -> Group.t -> unit) -> t -> unit

  (** Get the length. *)
  val length: t -> int
end

module GroupList: GROUP_LIST

