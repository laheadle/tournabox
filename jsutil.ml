
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

let textNode str = doc##createTextNode (Js.string str)

let getElementById_exn id = 
  let c = doc##getElementById (Js.string id) in
  Js.Opt.case c (fun () -> failwith ("no element with id " ^ id))
	(fun node -> node)

