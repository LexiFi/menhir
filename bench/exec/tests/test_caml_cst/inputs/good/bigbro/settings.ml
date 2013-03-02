(* $Header: /net/pauillac/caml/repository/bigbro/settings.ml,v 1.6 2004/05/05 09:44:46 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)

let version_number = "2.0.4";;

(*

This exception is raised when an invalid argument is encountered.

*)

exception Invalid of string

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Generic handlers for various kinds of arguments.

*)

let set_ref cell value =
  cell := value
;;

let set_ref_option cell value =
  cell := Some value
;;

let clear_ref_option cell () =
  cell := None
;;

let make_regexp flag value =
  try
    Pcre.regexp value
  with Failure message ->
    raise (Invalid ("Invalid " ^ flag ^ " regexp: " ^ message))
;;

let set_regexp flag cell value =
  cell := make_regexp flag value
;;

let set_regexp_option flag cell value =
  cell := Some (make_regexp flag value)
;;

let cons list value =
  list := value :: !list
;;

let cons_regexp flag cell value =
  cell := (make_regexp flag value) :: !cell
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling mappings.

*)

let mappings =
  ref ([] : (Pcre.regexp * Pcre.substitution) list)
;;

let last_mapfrom =
  ref None
;;

let mapfrom string =
  match !last_mapfrom with
    None ->
      set_regexp_option "-mapfrom" last_mapfrom string
  | Some _ ->
      raise (Invalid ("-mapfrom should be followed by -mapto"))
;;

let mapto string =
  match !last_mapfrom with
    None ->
      raise (Invalid ("-mapto should be preceded by -mapfrom"))
  | Some regexp ->
      mappings := (regexp, Pcre.subst string) :: !mappings;
      last_mapfrom := None
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling realms.

*)

let realms = ref ([] : (Pcre.regexp * string * string) list)
let last_realm = ref None
let last_user = ref None

let set_realm string = 
  match !last_realm with
    None ->
      set_regexp_option "-realm" last_realm string
  | Some _ ->
      raise (Invalid ("-realm should be followed by -user and -password"))
;;

let set_user string =
  match !last_realm with
    None ->
      raise (Invalid ("-user should be preceded by -realm"))
  | Some _ ->
      match !last_user with
    	None ->
	  last_user := Some string
      | Some _ ->
	  raise (Invalid ("Only one -user per -realm, please"))
;;

let set_password string =
  match !last_realm with
    None ->
      raise (Invalid ("-password should be preceded by -realm"))
  | Some realm ->
      match !last_user with
      	None ->
	  raise (Invalid ("-password should be preceded by -user"))
      | Some user ->
	  realms := (realm, user, string) :: !realms;
	  last_realm := None;
	  last_user := None
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Dealing with output options.

We can't print things unconditionally on standard output and let the caller deal with redirection, because we have
*several* output streams; one for raw information, one for HTML text, etc.

The functor below takes a string option, entered by the user on the command line, and converts it to a couple of
operations.

*)

module type OutputSig = sig

  val print: (unit -> string) -> unit
  val close: unit -> unit

end

module MakeOutput (Param : sig
		     val name: string option
		   end) = struct

  let nop _ = ()

  let print, close = match Param.name with
    None ->

      (* If no output has been requested, both operations are no-ops. *)

      nop, nop

  | Some "stdout" ->

      (* If the user has supplied the special keyword "stdout", then output is performed on standard output. *)

      let print deferred =
	print_string (deferred());
	flush stdout in

      print, nop

  | Some name ->

      (* Otherwise, direct output to the specified file. *)

      let channel = try
	open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_text ] 0o666 name
      with Sys_error message ->
	prerr_endline message;
	exit(1) in

      let print deferred =
	output_string channel (deferred());
	flush channel in

      let close () =
	close_out channel in

      print, close

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling the proxy specification.

*)

let proxy = ref None

let set_proxy text =
  try
    proxy := Some (Misc.name_and_port 80 text)
  with Misc.InvalidPort port ->
    raise (Invalid ("\"" ^ port ^ "\" is not a valid port number"))
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Default values.

*)

let local = ref false
let remote = ref false
let index = ref "index.html"
let local_html_files = ref (Pcre.regexp "\\.s?htm")
let max_threads = ref 16
let anonymous = ref []
let stdin = ref false
let recursion = ref None
let noproxy = ref None
let timeout = ref None
let gentle = ref None
let oraw = ref None
let ohtml = ref None
let odebug = ref None
let failures = ref false
let version = ref false
let fragments = ref false
let ignore = ref []

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This code computes our banner.

*)

let one_liner command =
  let channel = Unix.open_process_in command in
  let line = input_line channel in
  let _ = Unix.close_process_in channel in
  line
;;

let base_banner =
  Printf.sprintf "This is Big Brother %s under %s.\n"
    version_number
    (match Sys.os_type with
      "Unix" ->
        Printf.sprintf "%s %s (%s)"
          (one_liner "uname -s")
          (one_liner "uname -r")
          (one_liner "uname -m")
    | "Win32" ->
        "Windows"
    | "MacOS" ->
        "MacOS"
    | _ ->
        assert false
  )
;;

let banner =
  match Sys.os_type with
    "Win32" ->
      base_banner ^ "Written between 1996 and 2001 by Fran‡ois Pottier <Francois.Pottier@inria.fr>.\n"
  | "Unix" ->
      base_banner ^ "Written between 1996 and 2001 by François Pottier <Francois.Pottier@inria.fr>.\n"
  | _ ->
      assert false
;;

let html_banner =
  base_banner ^ "Written between 1996 and 2001 by <a href=\"mailto:Francois.Pottier@inria.fr\">François Pottier</a>.\n"
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The command line arguments are parsed here.

*)

let () = try
  Arg.parse [
    "-mapfrom", Arg.String mapfrom, "<regexp> defines the start of a mapping.";
    "-mapto", Arg.String mapto, "<replacement> defines the destination of a mapping.";
    "-rec", Arg.String (set_regexp_option "-rec" recursion), "<regexp> defines which URLs are recursively followed.";
    "-local", Arg.Set local, "enables checking local links.";
    "-remote", Arg.Set remote, "enables checking remote links.";
    "-index", Arg.String (set_ref index), "<string> specifies the name of index files.";
    "-localhtml", Arg.String (set_regexp "-local" local_html_files), "<regexp> tells which files are HTML files.";
    "-proxy", Arg.String set_proxy, "<string> specifies a proxy.";
    "-noproxy", Arg.String (set_regexp_option "-noproxy" noproxy), "<regexp> specifies where to bypass the proxy.";
    "-maxthreads", Arg.Int (set_ref max_threads), "<integer> specifies how many concurrent threads are allowed.";
    "-stdin", Arg.Set stdin, "specifies that links to be checked should be read from standard input.";
    "-timeout", Arg.Float (set_ref_option timeout), "<float> specifies a timeout value for network connections.";
    "-gentle", Arg.Float (set_ref_option gentle), "<float> specifies a minimum delay between hits to the same server.";
    "-realm", Arg.String set_realm, "<regexp> defines a password-protected realm.";
    "-user", Arg.String set_user, "<string> defines a user name for the last realm.";
    "-password", Arg.String set_password, "<string> defines a password for the last realm.";
    "-oraw", Arg.String (set_ref_option oraw), "<string> requests raw output and specifies where to send it.";
    "-ohtml", Arg.String (set_ref_option ohtml), "<string> requests HTML output and specifies where to send it.";
    "-odebug", Arg.String (set_ref_option odebug), "<string> enables debugging output and specifies where to send it.";
    "-failures", Arg.Set failures, "causes only failures to be reported.";
    "-fragments", Arg.Set fragments, "causes fragment identifiers (#there) to be checked as well.";
    "-ignore", Arg.String (cons_regexp "-ignore" ignore), "<regexp> specifies URLs to be ignored.";
    "-v", Arg.Set version, "prints a version number and exits."
  ] (cons anonymous) (banner ^ "Here is a list of all known switches:")
with Invalid message ->
  print_endline message;
  exit(1)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

If the user requested a version number, print it and exit.

*)

let () = if !version then begin
  print_string banner;
  flush stdout;
  exit(0)
end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

After we are done parsing, we can turn a few refs into immutable values.

We reverse the list of mappings so that mappings are stored in the same order as specified on the command line.
This might be important if the output of one mapping is an input of another one (something we don't recommend).

*)

let mappings = List.rev !mappings;;
let recursion = !recursion;;
let local = !local;;
let remote = !remote;;
let index = !index;;
let local_html_files = !local_html_files;;
let proxy = !proxy;;
let noproxy = !noproxy;;
let max_threads = !max_threads;;
let anonymous = !anonymous;;
let stdin = !stdin;;
let timeout = !timeout;;
let gentle = !gentle;;
let realms = !realms;;
let failures = !failures;;
let fragments = !fragments;;
let ignore = !ignore;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Output channels are created according to the user's specifications. First, we conduct a quick sanity check.

*)

let _ = match !oraw, !ohtml with
  None, None ->
    print_endline "You should specify at least one of -oraw/-ohtml. Use -help for help.";
    exit(1)
| _ ->
    ()

module RawOutput = MakeOutput (struct let name = !oraw end)
module HtmlOutput = MakeOutput (struct let name = !ohtml end)
module DebugOutput = MakeOutput (struct let name = !odebug end)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Other sanity checks.

*)

let _ = match local, remote with
  false, false ->
    print_endline "You must request -local, -remote or both.";
    exit(1)
| _ ->
    ()

let _ = if max_threads < 1 then begin
  print_endline "-maxthreads must be at least 1.";
  exit(1)
end

