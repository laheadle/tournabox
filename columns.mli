(**

   Columns, the basic units of visual information in a tournabox UI,
   are short lists of text chunks, each element of which may have css
   classes.

   Each column may specify whether or not filter text applies to it,
   that is, whether it is "filterable."

   Functions exist to render columns as HTML and to test them against
   filter text.

*)

(** Render an entry as a complex column, including country and
	seed. *)
val entry : ?filterable:bool -> Entry.t -> Ttypes.column

(** Get a column containing just a country name *)
val just_country : string -> Ttypes.column

(** Adds a header class to an existing column, thereby treating it as
	a header. *)
val as_header : Ttypes.column -> Ttypes.column

(** "In round N" *)
val in_round : int -> Ttypes.column

(** "advanced" *)
val advanced : Ttypes.column

(** "With a bye" *)
val with_a_bye : Ttypes.column

(** Renders either a "defeated" or "upset" column, based on seed
	disparity. *)
val defeated : winner:Entry.t -> Entry.t -> Ttypes.column

(** Renders either a "defeated" or "upset" column, based on seed
	disparity. *)
val was_defeated_by : winner:Entry.t -> Entry.t -> Ttypes.column

(** "will face" *)
val will_face : Ttypes.column

(** "to be decided" *)
val to_be_decided : Ttypes.column

(** Just a plain string *)
val plain : should_filter:bool -> string -> Ttypes.column
