module type Elt =
sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end
  
module type S =
sig
  type t
  type db
  val make_db: string list -> db
  val pick: string -> db -> t
  val to_string: t -> string
  val of_string: string -> t
end

module Make:
  functor (E: Elt) ->
	S with type t = E.t
	  and type db = E.t list

