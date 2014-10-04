
module Make: functor (Player: Player.S) ->
sig
  type t
  type data
  val to_string: t -> string
  val make: Player.t -> data -> t
  val parse: string -> (string * data)
end

