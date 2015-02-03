(**

   Columns, the basic units of visual information in a tournabox UI,
   are short lists of textual fragments, each element of which may
   have css classes. This allows us to do things like style the number
   "6" in the column "in round 6."

   Each column may specify whether or not filter text applies to it,
   that is, whether it is "filterable."

   Functions exist to render columns as HTML and to test them against
   filter text.

*)


(** A non-nested html span with a css class. *)
type shallow_span = {
  class_name: string;
  text: string;
}

(** The atomic element of a column. *)
type column_fragment =
  Text of string
| Elt of shallow_span

(** A column in the UI. *)
type column = {
  content: column_fragment list;

  class_name: string option;

  (** Whether filter text applies to this column *)
  should_filter: bool;
}

(** A header column of a group *)
type header_spec = {
  header: column;
  should_filter_header: bool;
}

(** Get The content of a column as a string *)
val column_content_string : column_fragment list -> string

val is_upset : column list -> bool

(** Render an entry as a complex column, including country and
	seed. *)
val entry : ?filterable:bool -> Entry.t -> column

(** Get a column containing just a country name *)
val just_country : string -> column

(** Adds a header class to an existing column, thereby treating it as
	a header. *)
val as_header : column -> column

(** "In round N" *)
val in_round : int -> column

(** "advanced" *)
val advanced : column

(** "With a bye" *)
val with_a_bye : column

(** Renders either a "defeated" or "upset" column, based on seed
	disparity. *)
val defeated : winner:Entry.t -> Entry.t -> column

(** Renders either a "defeated" or "upset" column, based on seed
	disparity. *)
val was_defeated_by : winner:Entry.t -> Entry.t -> column

(** "will face" *)
val will_face : column

(** "to be decided" *)
val to_be_decided : column

(** Just a plain string *)
val plain : should_filter:bool -> string -> column
