

Lwt_log_js.add_rule "*" Lwt_log_js.Error;
Lwt_log_js.add_rule "input" Lwt_log_js.Error;
Lwt_log_js.add_rule "filter" Lwt_log_js.Error;
Lwt_log_js.add_rule "playing" Lwt_log_js.Error;
Lwt_log_js.add_rule "grouping" Lwt_log_js.Error;
Lwt_log_js.default := Lwt_log_js.console

let debugf = Lwt_log_js.ign_debug_f
let errorf = Lwt_log_js.ign_error_f
let infof = Lwt_log_js.ign_info_f
let noticef = Lwt_log_js.ign_notice_f

let filter = Lwt_log_js.Section.make "filter"
let grouping = Lwt_log_js.Section.make "grouping"
let input = Lwt_log_js.Section.make "input"
let playing = Lwt_log_js.Section.make "playing"

