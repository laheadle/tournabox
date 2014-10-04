
module M: Player.S =
  struct
	type t = { name: string; country: string }

	let of_string str =
	  let len = String.length str in
	  let country = String.sub str (len - 3) 3 in
	  let name = String.sub str 0 (len - 3) in
	  { name = name; country = country }

	let to_string x = x.name ^ " " ^ x.country
  end

