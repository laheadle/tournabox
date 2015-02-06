(** The supported {! Group.grouping_spec} objects.

    	Each tournabox has one or more of them, depending on the value of the
    	tournabox-groups container attribute.

    	performance and seed are "player groups," meaning their headers
    	are a player's name and their contests are that player's
    	contests. *)

(** The "By Country" group *)
val country: Group.grouping_spec

(** The "By Round" group *)
val round: Group.grouping_spec

(** The "By Performance" group *)
val performance: Group.grouping_spec

(** The "By Seed" group *)
val seed: Group.grouping_spec
