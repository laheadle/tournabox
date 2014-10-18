
module Make: functor (Player: Player.S) ->
sig
  type t
  type data
  type player
  type column = string * string option

  val to_string: t -> string
  val make: Player.t -> data -> t
  val parse: string -> (string * data)

  val entry_specs: (t, t) Ttypes.converted_grouping_spec list
  val player_specs: (player, t) Ttypes.converted_grouping_spec list
end

