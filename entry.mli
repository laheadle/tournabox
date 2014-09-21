
module type S =
  sig
	type t
	type db
	val to_string: t -> string
	val make_entries: db -> string list -> t list
  end

module Make:
  functor (Player: Player.S) ->
      S with type db = Player.db
