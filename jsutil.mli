(** General Javascript utilities *)

open Js

(** The Document *)
val doc : Dom_html.document t

(** Get an element's class name *)
val get_classname : #Dom_html.element t -> string

(** Set an element's class name; pass None for none. *)
val set_classname : #Dom_html.element t -> string option -> unit

(** Create a table with a given class name *)
val table : string option -> Dom_html.tableElement t

(** Create a text node with the given text *)
val textNode : string -> Dom.text t

(** Add a td to a table, with text content and a class name *)
val addTd : Dom_html.tableElement t -> string -> string option -> unit

(** Get an element with an id, or raise failure *)
val getElementById_exn : string -> Dom_html.element t

(** Get an element's attribute, or raise Not_found *)
val getAttribute_exn : #Dom_html.element t -> string -> string

(** Get the children of an element *)
val child_elements : #Dom_html.element t -> Dom_html.element Js.t list

(** Raised by text_of *)
exception Not_text
exception No_children

(** Used for getting the raw text of DOM nodes containing entries or
	outcomes. *)
val text_of : #Dom.node t -> string

(** Get the y offset of an element. Used for scrolling to an element's
	position *)
val offset_of : Dom_html.element t -> int

(** Delete the children of a Dom node *)
val delete_children : #Dom.node Js.t -> unit
