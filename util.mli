
(** Various utility functions *)


(** unwraps an option *)
val get_option : 'a option -> 'a

(** map a function over an option *)
val map_option : ('a -> 'b) -> 'a option -> 'b option

(** Return true for Some x *)
val is_some: 'a option -> bool

(** Return true for None *)
val is_none: 'a option -> bool

(** [pow a b] returns a to the bth power *)
val pow : int -> int -> int

(** [replace arr i f] replaces arr.(i) with f(arr.(i)) *)
val replace : 'a array -> int -> ('a -> 'a) -> unit

(** return true if the argument is a power of two *)
val power_of_two : int -> bool

(** [log a b] returns log base a of b *)
val log : int -> int -> int

(** searches for a string within another string *)
val contains : within:string -> string -> bool

(** Strip spaces from the front and back *)
val strip_spaces : string -> string

(** Head of a list, or assertion failure *)
val hd_exn : 'a list -> 'a

(** [pick list partial to_string] returns the element of list that
	matches partial. to_string is called to get the string representation
	of each member of list. *)
val pick : 'a list -> string -> ('a -> string) -> 'a

(** The identity function *)
val id : 'a -> 'a

(** First filter a list, then map over it *)
val filter_then_map :
  mapf:('a -> 'b) -> filterf:('a -> bool) -> 'a list -> 'b list

