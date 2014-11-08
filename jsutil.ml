
let doc = Dom_html.document

let set_classname elt className =
  match className with
	None -> ()
  | Some c -> elt##className <- (Js.string c)

let table className = 
	let table = Dom_html.createTable doc in
	set_classname table className;
	table

let textNode str = doc##createTextNode (Js.string str)

let addTd tr str className =
  let td = Dom_html.createTd doc in
  set_classname td className;
  Dom.appendChild td (textNode str);
  Dom.appendChild tr td 

let getElementById_exn id = 
  let c = doc##getElementById (Js.string id) in
  Js.Opt.case c (fun () -> failwith ("There is no element with id " ^ id))
	(fun node -> node)

let getAttribute_exn node attr =
  let opt = node##getAttribute (Js.string attr) in
  let jopt = Js.Opt.get opt (fun _ -> raise Not_found) in
  Js.to_string jopt
