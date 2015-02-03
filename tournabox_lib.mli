(** This is the top-level tournabox library. Clients can use these
	types and values to find and run tournaboxes. *)

(** An element that will contain a visible tournabox. *)
type container = Dom_html.element Js.t

(** A parsed container. The argument to run/play a tournabox. *)
type tourney_shell

(** Get a list of all the containers on the page *)
val get_all_containers : unit -> container list

(** Given a container, return a tourney shell or None and an error
	message *)
val get_tourney_shell : container -> tourney_shell option * string

(** Play and display a tourney_shell *)
val play : tourney_shell -> unit

(** Get all tourney shell options on a page, with error messages. *)
val get_all_tourney_shells : unit -> (tourney_shell option * string) list

