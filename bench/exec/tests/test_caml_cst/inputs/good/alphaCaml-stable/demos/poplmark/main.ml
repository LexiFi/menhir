
(* Parse the data found on the standard input channel. *)

let raw_toplevel =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.toplevel Lexer.main lexbuf
  with Parsing.Parse_error ->
    Printf.eprintf "Syntax error near character %d.\n%!" (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum);
    exit 1

(* Turn it into internal form. *)

let toplevel =
  let empty = Fsub.Identifier.Map.empty in
  try
    Fsub.import_toplevel (empty, empty) raw_toplevel
  with
  | Fsub.Type_var.UnboundIdentifier x ->
      Printf.eprintf "Unbound type variable: %s.\n%!" x;
      exit 1
  | Fsub.Term_var.UnboundIdentifier x ->
      Printf.eprintf "Unbound term variable: %s.\n%!" x;
      exit 1

(* Process it. *)

let rec process env ctx (term_ids, type_ids) = function
  | Fsub.TopEOF ->
      ()
  | Fsub.TopEval (t, rest) ->
      begin
	try
	  let tyT = Core.typeof env ctx t in
	  let t' = Core.eval t in
	  Printer.printtm (Fsub.export_term_expr (term_ids, type_ids) t');
	  Format.print_break 1 2;
	  Format.print_string ": ";
	  Printer.printty (Fsub.export_type_expr type_ids tyT)
	with
	| Failure msg ->
	    print_string msg
	| Core.NoRuleApplies ->
	    print_string "error: no evaluation rule applies"
      end;
      Format.force_newline();
      process env ctx (term_ids, type_ids) rest
  | Fsub.TopTermBind abs ->
      let x, tyT, rest = Fsub.open_term_top_abs abs in
      let env = Fsub.Term_var.AtomMap.add x tyT env in
      let term_ids = Fsub.Term_var.AtomIdMap.add x term_ids in
      Format.print_string (Fsub.Term_var.AtomIdMap.lookup x term_ids);
      Format.print_break 1 2;
      Format.print_string ": ";
      Printer.printty (Fsub.export_type_expr type_ids tyT);
      Format.force_newline();
      process env ctx (term_ids, type_ids) rest
  | Fsub.TopTypeBind abs ->
      let tyX, tyT, rest = Fsub.open_type_top_abs abs in
      let ctx = Fsub.Type_var.AtomMap.add tyX tyT ctx in
      let type_ids = Fsub.Type_var.AtomIdMap.add tyX type_ids in
      Format.print_string (Fsub.Type_var.AtomIdMap.lookup tyX type_ids);
      Format.print_break 1 2;
      Format.print_string "<: ";
      Printer.printty (Fsub.export_type_expr type_ids tyT);
      Format.force_newline();
      process env ctx (term_ids, type_ids) rest

let no_term_ids =
  Fsub.Term_var.AtomIdMap.empty

let no_type_ids =
  Fsub.Type_var.AtomIdMap.empty

let () =
  Format.set_max_boxes 1000;
  Format.set_margin 67;
  Format.open_hvbox 0;
  process Core.noenv Core.noctx (no_term_ids, no_type_ids) toplevel;
  Format.print_flush()

