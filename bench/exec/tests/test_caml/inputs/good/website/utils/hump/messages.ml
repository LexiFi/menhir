let _ = Config.dbg "Entering module messages.ml"


(** Messages *)

(** {2 Command line options} *)

let usage ="Usage : "^Sys.argv.(0)^" [options]"
let options_are = "where options are :"

(* Please keep options in alphabetical order. *)

let option_d = "<db>  connect to database <db> ; "^
	       "default is "^Config.db_name

let option_h = "<host>  connect to the database on <host> ; "^
	       "default is "^Config.db_host

let option_p = "<passwd>  use <passwd> as password to connect to the database ; "^
	       "default is "^Config.db_passwd

let option_u = "<user>  connect to the database as <user> ; "^
	       "default is "^Config.db_user

let option_v = " verbose mode"

(** {2 Error messages} *)

let error = "Error"
let err_not_implemented = "not implemented yet !!"

let err_dont_call_the_cgi_this_way = "This cgi cannot be called this way."
let err_not_connected = "Not connected"
let err_exec_com com = "Error while executing the command "^com
let err_open_file name = "Could not read file "^name

let err_invalid_login = "Invalid login."
let err_login_already_exists s = 
  Printf.sprintf "Login %s already exists." s
let err_author_already_exists = 
  "An author with that name already exists."

let err_no_id = "No id !"
let err_no_idprop = "No idprop !"
let err_no_kind = "No kind !"

let err_too_big = "Query too big.<br/>"^
		  "Click on \"Back\" to get back."

let err_must_be_connected = "You must be connected."
let err_invalid_password = "Bad password."
let err_unknown_user login = Printf.sprintf "Unknown user \"%s\"" login
let err_unknown_iduser id = Printf.sprintf "Unknown user id %d." id

let login_mandatory = "Login is mandatory."
let passwd_mandatory = "Password is mandatory."
let name_mandatory = "Name is mandatory."
let icon_mandatory = "Icon is mandatory."
let prop_father_mandatory = "Father property is mandatory."
let prop_mandatory = "Property is mandatory."
let author_mandatory = "Author is mandatory."
let url_mandatory = "Url is mandatory."
let description_mandatory = "Description is mandatory."
let status_mandatory = "Status is mandatory."

(** {2 Types} *)

let of_prop_kind = function
    Types.Kind -> "Kind"
  | Types.License -> "License"
  | Types.Topic -> "Topic"
  | Types.Attribute -> "Attribute"
    

(** {2 Status} *)

let no_status = "No status"

let of_status = function
  | Types.Pre_alpha -> "Development code"
  | Types.Alpha -> "Alpha"
  | Types.Beta -> "Beta"
  | Types.Stable -> "Stable"
  | Types.Mature -> "Mature"

let of_status_opt = function
    None -> no_status
  | Some s -> of_status s

(** {2 Messages / labels} *)

let std_title = "The Caml Humps"
let login_title = "Login"
let welcome = "Welcome"
let id = "id"
let yes = "Yes"
let no = "No"
let confirmation = "Confirmation"
let connection = "Connection"
let connected_as s = Printf.sprintf "Connected as %s" s
let not_connected = "Not connected"
let disconnection = "Logout"
let other = "Other"
let others = "Others"
let passwd = "Password"
let login = "Login"
let mysql = "MySQL"
let execute = "execute"
let query = "Query"
let results = "Results"
let submit = "Submit"
let firstname = "Firstname"
let name = "Name"
let mail = "Mail"
let home = "Home"
let url = "URL"
let homepage_indexed = "Homepage indexed"
let icon = "Icon"
let style = "Style"
let mysql_no_results = "La requête s'est executée correctement (Pas de lignes retournées)."
let remove = "Remove"
let none = "None"
let initialization = "Initialization"
let cancel = "Cancel"
let update = "Update"
let add = "Add"
let modify = "Modify"
let remove = "Remove"
let add_author = "Add author"
let modify_author = "Modify author"
let authors = "Authors"
let add_contrib = "Add contrib"
let modify_contrib = "Modify contrib"
let contribs = "Contribs"
let really_remove_author s = Printf.sprintf "Really remove author \"%s\" ?" s
let properties = "Properties"
let add_property kind = 
  Printf.sprintf "Add %s" (of_prop_kind kind)
let modify_property = "Modify property"

let prop_father = "Father property"
let really_remove_property s =
  Printf.sprintf "Really remove property \"%s\" ?" s

let link_kinds = "Link kinds"
let add_link_kind = "Add link kind"
let modify_link_kind = "Modify link kind"
let really_remove_link_kind s = 
  Printf.sprintf "Really remove link kind \"%s\" ?" s

let really_remove_contrib s = 
  Printf.sprintf "Really remove contrib \"%s\" ?" s

let version = "Version"
let date = "Date"
let description = "Description"
let set_date = "Set date"
let property = "Property"
let status = "Status"

let sort_by = "Sort by"
let select_view = "Select view"
let browse = "Browse"

let contrib_names = "names"
let contrib_dates = "dates"
let contrib_kinds = "Types"
let contrib_status = "status"

let related_topics = "Related topics"
let topics = "Topics"
let latest_updates = "Latest updates"

let recent_contribs = "Recently updated contribs"
let old_contribs = "Older contribs"

let in_topic t = Printf.sprintf "in %s" t
let by_author a = Printf.sprintf "by %s" a
let by_ t = Printf.sprintf "by %s" t

(** {2 Dates} *)

let jan = "Jan"
let feb = "Feb"
let mar = "Mar"
let apr = "Apr"
let may = "May"
let jun = "Jun"
let jul = "Jul"
let aug = "Aug"
let sep = "Sep"
let oct = "Oct"
let nov = "Nov"
let dec = "Dec"

let sunday = "Sunday"
let monday = "Monday"
let tuesday = "Tuesday"
let wednesday = "Wednesday"
let thursday = "Thursday"
let friday = "Friday"
let saturday = "Saturday"

let string_of_date ?(wday=false) ?(hours=true) ?(secs=false) d =
  let tm = Unix.localtime d in
  let mon =
    match tm.Unix.tm_mon with
      0 -> jan
    | 1 -> feb
    | 2 -> mar
    | 3 -> apr
    | 4 -> may
    | 5 -> jun
    | 6 -> jul
    | 7 -> aug
    | 8 -> sep
    | 9 -> oct
    | 10 -> nov
    | _ -> dec
  in
  let day =
    match tm.Unix.tm_wday with
      0 -> sunday
    | 1 -> monday
    | 2 -> tuesday
    | 3 -> wednesday
    | 4 -> thursday
    | 5 -> friday
    | _ -> saturday
  in
  Printf.sprintf
    "%s%02d-%s-%4d%s"
    (if wday then day^", " else "")
    tm.Unix.tm_mday mon (tm.Unix.tm_year + 1900)
    (
      if hours then
	Printf.sprintf  " %02d:%02d%s"
          tm.Unix.tm_hour tm.Unix.tm_min 
          (if secs then Printf.sprintf ":%02d" tm.Unix.tm_sec else "")
      else
	""
    )

