
(* Parse the data found on the standard input channel. *)

let raw_expression =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.toplevel Lexer.main lexbuf
  with Parsing.Parse_error ->
    Printf.eprintf "Syntax error near character %d.\n" (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum);
    flush stderr;
    exit 1

(* Turn it into internal form. *)

let e =
  Mm.import_expression Mm.Identifier.Map.empty raw_expression

(* Process it. *)

let rec process e =
  assert (Mm.Var.AtomSet.is_empty (Mm.free_expression e)); (* sanity check *)
  Printer.print_expression (Mm.export_expression Mm.Var.AtomIdMap.empty e);
  try
    let e = Core.eval e in
    Format.force_newline();
    Format.print_string "... reduces to ...";
    Format.force_newline();
    process e
  with Core.Value ->
    ()

let () =
  Format.set_max_boxes 1000;
  Format.set_margin 67;
  Format.open_hovbox 0;
  process e;
  Format.print_newline()

