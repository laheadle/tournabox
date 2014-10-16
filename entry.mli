
module Make: functor (Player: Player.S) ->
sig
  type t
  type data
  type player
  type column = string * string option

  type 'a grouping_spec = {
	name:string;
	header_name: num_rounds:int -> pos:int -> 'a Choice.t list -> string;
	compare_choice: 'a Choice.t -> 'a Choice.t -> int;
	compare_group: 'a Choice.t list -> 'a Choice.t list -> int;
	in_group: 'a Choice.t -> 'a Choice.t list -> Player.group_result;
	column_extractor: int -> int -> 'a Choice.t -> column list;
	convert: t Choice.t -> 'a Choice.t;
  }

  val to_string: t -> string
  val make: Player.t -> data -> t
  val parse: string -> (string * data)

  val player_specs: player grouping_spec list
end

