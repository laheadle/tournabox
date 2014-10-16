module type S =
sig
  type t

  type group_result =  {
	quit: bool;
	this_group: bool;
  }
  type column = string * string option

  type grouping_spec = {
	name:string;
	header_name: num_rounds:int -> pos:int -> t Choice.t list -> string;
	compare_choice: t Choice.t -> t Choice.t -> int;
	compare_group: t Choice.t list -> t Choice.t list -> int;
	in_group: t Choice.t -> t Choice.t list -> group_result;
	column_extractor: int -> int -> t Choice.t -> column list;
  }

  val to_string: t -> string
  val of_string: string -> t
  val grouping_specs: grouping_spec list
end
