let _ = Config.dbg "Entering module constant.ml"



let random_string_length = 15
let id_other = -1
let s_id_other = string_of_int id_other

let default_style_css = "default"

let first_sentence_limit = 100

let latest_updates_number = 10

(* callbacks *)

let cb_create_tables = "createtables"
let cb_info = "info"
let cb_init = "init"
let cb_form_login = "formlogin"
let cb_login = "login"
let cb_logout = "logout"
let cb_mysql = "mysql"

let cb_admin_authors = "authors"
let cb_form_add_author = "formaddauthor"
let cb_add_author = "addauthor"
let cb_form_modify_author = "formmodifyauthor"
let cb_modify_author = "modifyauthor"
let cb_remove_author = "removeauthor"

let cb_admin_contribs = "contribs"
let cb_form_add_contrib = "formaddcontrib"
let cb_add_contrib = "addcontrib"
let cb_form_modify_contrib = "formmodifycontrib"
let cb_modify_contrib = "modifycontrib"
let cb_remove_contrib = "removecontrib"
let cb_set_contrib_date = "setcontribdate"
let cb_add_contrib_prop = "addcontribprop"
let cb_remove_contrib_prop = "removecontribprop"
let cb_add_contrib_author = "addcontribauthor"
let cb_remove_contrib_author = "removecontribauthor"

let cb_admin_properties = "properties"
let cb_add_property = "addproperty"
let cb_form_add_property = "formaddproperty"
let cb_form_modify_property = "formmodifyproperty"
let cb_modify_property = "modifyproperty"
let cb_remove_property = "removeproperty"

let cb_admin_link_kinds = "linkkinds"
let cb_form_add_link_kind = "formaddlinkkind"
let cb_add_link_kind = "addlinkkind"
let cb_form_modify_link_kind = "formmodifylinkkind"
let cb_modify_link_kind = "modifylinkkind"
let cb_remove_link_kind = "removelinkkind"


(* names of parameters variables *)

let var_id = "id"
let var_idprop = "idprop"
let var_idauthor = "idauthor"
let var_confirm = "confirm"
let var_name = "name"
let var_kind = "kind"
let var_contrib = "contrib"
let var_letter = "letter"
