open Ocamlbuild_plugin
open Command

(* ---------------------------------------------------------------------------- *)

(* Compilation flags. *)

let flags () =
  (* -inline 1000 *)
  flag ["ocaml"; "compile"; "native"] (S [A "-inline"; A "1000"]);
  (* -noassert *)
  flag ["ocaml"; "compile"; "noassert"] (S [A "-noassert"]);
  (* nazi warnings *)
  flag ["ocaml"; "compile"; "my_warnings"] (S[A "-w"; A "@1..49-4-9-41-44"])

(* ---------------------------------------------------------------------------- *)

(* Dealing with the two parsers. *)

(* Just for fun, Menhir comes with two parsers for its own input files. One is
   called [yacc-parser.mly] and is built using [ocamlyacc]. The other is called
   [fancy-parser.mly] and is built using Menhir. It depends on [standard.mly].
   The choice between the two parsers is determined by the presence of the tag
   [fancy_parser]. *)

let fancy () : bool =
  mark_tag_used "fancy_parser";
  Tags.mem "fancy_parser" (tags_of_pathname "")

let compile_messages grammar messages target =
  rule
    "compile a custom table of messages"
    ~prod:target
    ~deps:[ grammar; messages ]
    (fun env _ ->
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        (* no additional flags; may allow them in the future *)
        P (env grammar);
        A "--compile-errors"; P (env messages);
        Sh ">"; Px (env target);
      ]))

let parser_configuration () =
  (* Create [parser.mly] by copying the appropriate source file. *)
  copy_rule "create parser.mly"
    (* source: *)
    (if fancy() then "fancy-parser.mly" else "yacc-parser.mly")
    (* target: *)
    "parser.mly"
  ;
  (* Create [Driver.ml] by copying the appropriate source file. *)
  copy_rule "create Driver.ml" 
    (* source: *)
    (if fancy() then "fancyDriver.ml" else "yaccDriver.ml")
    (* target: *)
    "Driver.ml"
  ;
  (* In the fancy case, use Menhir to generate [FancyParserMessages.ml] based
     on [fancy-parser.messages], which is maintained by hand. *)
  if fancy() then
    compile_messages
      (* sources: *)
      "parser.mly" "fancy-parser.messages"
      (* target: *)
      "FancyParserMessages.ml"

(* ---------------------------------------------------------------------------- *)

(* Define custom compilation rules. *)

let () =
  dispatch (function After_rules ->
    (* Add our rules after the standard ones. *)
    parser_configuration();
    flags();
  | _ -> ()
  )
