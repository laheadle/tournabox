
(* non-nested html span *)
type shallow_span = {
  class_name: string;
  text: string;
}

type column_fragment =
  Text of string
| Elt of shallow_span

type column = {
  content: column_fragment list;
  class_name: string option;
  should_filter: bool;
}

let column_content_string fragments =
  let f = function | Text s -> s | Elt {text} -> text in
  let strings = List.map f fragments in
  List.fold_left (^) "" strings

type header_spec = {
  header: column;
  should_filter_header: bool;
}

let is_upset columns =
  let test = function
      Text "upset" -> true
    | Text "was upset by" -> true
    | _ -> false
  in
  let is_upset frag_list =
    List.exists test frag_list
  in
  List.exists is_upset (List.map (fun col -> col.content) columns)

module Classes =
struct
  let name = "tournabox-name"
  let country = "tournabox-country"
  let seed ="tournabox-seed"
  let entry = "tournabox-entry"
  let seeded ="tournabox-seeded"
  let unseeded ="tournabox-unseeded"
  let text ="tournabox-text"
  let defeated ="tournabox-defeated"
  let advanced ="tournabox-advanced"
  let was_defeated_by ="tournabox-was-defeated-by"
  let will_face="tournabox-will-face"
  let to_be_decided = "tournabox-to-be-decided"
  let in_round = "tournabox-in-round"
  let round = "tournabox-round"
  let upset = "tournabox-upset"
  let was_upset_by = "tournabox-was-upset-by"
  let just_country = "tournabox-just-country"
  let with_a_bye = "tournabox-with-a-bye"
end

let entry ?(filterable=true) e =
  let (class_name, seed) = 
	match e.Entry.seed with
	  Some s ->
		Classes.seeded, [Elt {class_name=Classes.seed;text=string_of_int s}]
	| _ -> Classes.unseeded, []
  in
  let country =
	match e.Entry.country with
	  None -> []
	| Some c -> [ Elt {class_name=Classes.country;text=c} ]
  in
  let content=[
	Elt {class_name=Classes.name;text=e.Entry.player}
  ] @ country @ seed
  in  {
	content;
	class_name=Some (Classes.entry ^ " " ^ class_name);
	should_filter=filterable
  }

let just_country c = {
  content=[Text c];
  class_name=Some (Classes.just_country);
  should_filter=true;
}

let as_header =
  let mk column str =
	{ column with class_name = Some (str ^ " tournabox-header") }
  in function
  | { class_name=Some str} as column -> mk column str
  | { class_name=None} as column -> mk column ""

let in_round r =  {
  content=[
	Text "In round";
	Elt {
	  class_name=Classes.round;
	  text=string_of_int r;
	}
  ];
  class_name=Some Classes.in_round;
  should_filter=false
}

let advanced = {
  content=[Text "advanced"];
  class_name=Some Classes.advanced;
  should_filter=false
}

let with_a_bye = {
  content=[Text "with a bye"];
  class_name=Some Classes.with_a_bye;
  should_filter=false
}

let defeated ~winner loser =
  let upset = 	{
	  content=[Text "upset"];
	  class_name=Some Classes.upset;
	  should_filter=false
	}
  in
  match winner.Entry.seed, loser.Entry.seed with
	None, Some _ -> upset
  | Some a, Some b when a > b -> upset
  | _ ->
	{
	  content=[Text "defeated"];
	  class_name=Some Classes.defeated;
	  should_filter=false
	}

let was_defeated_by ~winner loser =
  let upset = 	{
	  content=[Text "was upset by"];
	  class_name=Some Classes.was_upset_by;
	  should_filter=false
	}
  in
  match winner.Entry.seed, loser.Entry.seed with
	None, Some _ -> upset
  | Some a, Some b when a > b -> upset
  | _ ->
	{
	  content=[Text "was defeated by"];
	  class_name=Some Classes.was_defeated_by;
	  should_filter=false
	}


let will_face = {
  content=[Text "will face"];
  class_name=Some Classes.will_face;
  should_filter=false
}

let to_be_decided = {
  content=[Text "To be decided"];
  class_name=Some Classes.to_be_decided;
  should_filter=false
}

let plain ~should_filter txt =  {
  content=[Text txt];
  class_name=None;
  should_filter;
}
