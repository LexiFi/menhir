open Ocamlbuild_plugin
open Command

(* Dealing with the two parsers. *)

let fancy () : bool =
  mark_tag_used "fancy";
  Tags.mem "fancy" (tags_of_pathname "")

let parser_configuration () =
  (* Create [parser.mly] by copying the appropriate source file. *)
  copy_rule "create parser.mly"
    (* source: *)
    (if fancy() then "parser-menhir.mly" else "parser-ocamlyacc.mly")
    (* target: *)
    "parser.mly"

let () =
  dispatch (function After_rules ->
    parser_configuration();
  | _ -> ()
  )

