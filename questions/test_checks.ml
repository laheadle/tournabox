let do_checks positive check_boxes =
  positive##checked <- Js._true;
  List.iter (fun negative ->negative##checked <- Js._false)
    (List.filter (fun check -> check <> positive ) check_boxes)

let clicks check =
  Lwt.bind
    (Lwt_js_events.click check)
    (fun _ -> Lwt.return check)

let rec main_loop checks =
  let threads = List.map clicks checks in
  let triggered = Lwt.pick threads in
  Lwt.bind triggered (fun check ->
      ignore (do_checks check checks);
      main_loop checks)

let doc = Dom_html.document
let add =  Dom.appendChild
let div = Jsutil.getElementById_exn "container"

let make () =
  let check = Dom_html.createInput ~_type:(Js.string "checkbox") doc in
  add div check;
  check

let checks = [make (); make(); make()]

let () =
  ignore (main_loop checks)


