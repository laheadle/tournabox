
let doc = Dom_html.document

let table className = 
	let table = Dom_html.createTable doc in
	(match className with
	  None -> ()
	| Some c -> table##className <- (Js.string c));
	table

let addTd tr str className =
  let td = Dom_html.createTd doc in
  (match className with
	None -> ()
  | Some c -> td##className <- (Js.string c));
  Dom.appendChild td (doc##createTextNode (Js.string str));
  Dom.appendChild tr td 
