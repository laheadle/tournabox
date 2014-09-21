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

module Make(E: Elt) =
  struct
	type t = E.t
	type db = E.t list

	let to_string = E.to_string
	let of_string = E.of_string
	let make_db list = List.map of_string list
	let pick str db = Util.pick db str to_string
  end
