let _ = Config.dbg "Entering module model.ml"



module A = Args

open Ocgi


module Session_model =
struct
  type t = Db.Users.t
  type html = Ocgi.Html.html
  type session_id = string

  let string_of_html xml = xml

  let string_of_session_id s = s
  let session_id_of_string s = s

  let failure_handler mes = (* TODO *)
    failwith 
      (Printf.sprintf
	 "Model: Session_model.failure_handler not implemented<br/>\nMessage is %s"
	 mes
      )

  let get_user env sid =
    Sessions.get_user env.Env.remote_host sid

  let callback_varname = "callback"
  let session_varname = Config.session_varname
end

module M = Ocgi.Model.FCgi_session_cookies (Session_model)


let exception_page e =
  match e with
    Ocgi.Env.Query_string_too_big ->
      Ocgi.Html.page Messages.error Messages.err_too_big
  | Failure s ->
      Ocgi.Html.page Messages.error s
  | e ->
      Ocgi.Html.page Messages.error (Printexc.to_string e)

let _ = Ocgi.Hooks.exception_page := exception_page
let _ = Ocgi.Hooks.content_type := "text/pre-xhtml"

let main callbacks cookie_callbacks default =
  try
    A.parse ();
    List.iter (fun (n, f) -> M.register n f) callbacks;
    List.iter (fun (n, f) -> M.cookie_register n f) cookie_callbacks;
    M.main ~default
  with
  | Failure s ->
      prerr_endline s;
      exit 1

let connected cb env user_opt =
  match user_opt with
    None ->
      Page.admin_error 
	env
	user_opt
	Messages.err_must_be_connected

  | Some u -> 
      cb u env user_opt


