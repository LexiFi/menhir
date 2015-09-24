open Ocamlbuild_plugin
open Command

(* ---------------------------------------------------------------------------- *)

(* Compilation flags. *)

let flags () =
  (* -inline 1000 *)
  flag ["ocaml"; "compile"; "native"] (S [A "-inline"; A "1000"]);
  (* nazi warnings *)
  flag ["ocaml"; "compile"; "my_warnings"] (S[A "-w"; A "@1..49-4-9-41-44"])

(* ---------------------------------------------------------------------------- *)

(* Dealing with the two parsers. *)

(* Just for fun, Menhir comes with two parsers for its own input files. One is
   called [yacc-parser.mly] and is built using [ocamlyacc]. The other is called
   [fancy-parser.mly] and is built using Menhir. It depends on [standard.mly].
   The choice between the two parsers is determined by the presence of the tag
   [fancy_parser]. *)

let parser_rule () =
  let source =
    mark_tag_used "fancy_parser";
    if Tags.mem "fancy_parser" (tags_of_pathname "")
    then "fancy-parser.mly"
    else "yacc-parser.mly"
  in
  copy_rule "create parser.mly" source "parser.mly"

(* ---------------------------------------------------------------------------- *)

(* Define custom compilation rules. *)

let () =
  dispatch (function After_rules ->
    (* Add our rules after the standard ones. *)
    parser_rule();
    flags();
  | _ -> ()
  )
