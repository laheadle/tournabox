
(*
Hello,

If I try to compile this:

  let doc = Dom_html.document in
  let containers = doc##body#querySelectorAll ".tourney-container" in
  [whatever]

I get:

Error: This expression has type Dom_html.bodyElement Js.t
       It has no method querySelectorAll

Now, every bodyElement is a nodeSelector, because it is an element. It therefore has the method querySelectorAll. So it seems the problem is how to go from a bodyElement Js.t to a bodyElement. How do I do that (or otherwise do what I want)?

*)

(*
let containers = doc##body##querySelectorAll (Js.string ".tourney-container") in
5*)

let doc = Dom_html.document
let assF = (fun () -> assert false)

let () =
  let all = ref [] in
  let firstChild node =
	Js.Opt.get (node##childNodes##item(0)) assF in
  let textChild node =
	let child = firstChild node in
	let opt = Dom.CoerceTo.text child in
	Js.Opt.get opt assF in
  let text_of node =
	(textChild node)##data in
  let containers = doc##body##querySelectorAll (Js.string ".tourney-container") in
  let lst = Dom.list_of_nodeList containers in
  let mapped = List.map (fun node ->
	let id = Js.to_string node##id in
	let entries = Jsutil.getElementById_exn (id ^ "-entries") in
	let outcomes = Jsutil.getElementById_exn (id ^ "-outcomes") in
	(text_of entries, text_of outcomes, node))
	lst in
  Printf.printf "%d found" (List.length mapped)
