module type S =
sig
  type t

  val to_string: t -> string
  val of_string: string -> t
  val grouping_specs: t Ttypes.grouping_spec list
end
