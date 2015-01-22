
type container = Dom_html.element Js.t

type tourney_shell = {
  entries : string;
  outcomes : string;
  chosen_specs : Tourney.grouping_spec list;
  hide_menubar : bool;
  filters_requested : string;
  container : container
}

val play : tourney_shell -> unit

val get_tourney_shell : container -> tourney_shell option * string

val get_all_containers : unit -> container list

val get_all_tourney_shells : unit -> (tourney_shell option * string) list

