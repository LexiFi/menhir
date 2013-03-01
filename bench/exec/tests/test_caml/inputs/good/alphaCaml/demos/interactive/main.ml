(* To begin, set up a function that reads a line of input and parses
   it. If lexing or parsing fails, the function displays an error
   message and starts over, so that failure is not observable. *)

let rec parse_declarations () : Syntax.Raw.declarations =
  (* Print a prompt. *)
  print_string "# ";
  flush stdout;
  (* Read a line. This will raise [End_of_file] if the user hits ^D. *)
  let lexbuf = Lexing.from_string (input_line stdin) in
  (* Parse that line, and, if successful, return the raw parse tree. *)
  try
    Parser.declarations Lexer.main lexbuf
  with
  (* Handle errors by printing an error message and retrying. *)
  | Parsing.Parse_error ->
      print_string "Sorry -- parse error: expected \"<variable> = <expression>\".\n";
      flush stdout;
      parse_declarations()
  | Lexer.Lexical_error c ->
      Printf.printf "Sorry -- lexical error at character %d.\n" c;
      flush stdout;
      parse_declarations()

(* Tie the recursive knot. The function [declarations] above
   is the one that the parser should refer to when building
   a suspension that reads more input. So, write this function
   into the appropriate reference. *)

let () =
  ParserFix.declarations := parse_declarations

(* An evaluator for expressions. This code is perfectly normal. The
   language is so simple that, in the absence of [EFail], no failure
   would be possible. This explains why I introduced this special
   expression, whose effect is to artificially cause an evaluation
   failure. *)

exception EvaluationFailure

type env = Syntax.value Syntax.Var.AtomMap.t

let rec eval (env : env) : Syntax.expr -> Syntax.value = function
  | Syntax.EVar x ->
      Syntax.Var.AtomMap.lookup x env
  | Syntax.EConst k ->
      Syntax.VConst k
  | Syntax.EAdd (e1, e2) ->
      begin
	match eval env e1, eval env e2 with
	| Syntax.VConst k1, Syntax.VConst k2 ->
	    Syntax.VConst (k1 + k2)
      end
  | Syntax.EFail ->
      raise EvaluationFailure

(* Now comes the main processing loop. This is where expressions are
   evaluated under an evaluation environment that grows with every
   declaration. *)

type suspension = Syntax.declarations Suspension.t
type esuspension = Syntax.evaluated_declarations Suspension.t

(* [process env suspension] turns a stream of declarations,
   represented by the parameter [suspension], into a stream of
   evaluated declarations.

   The parameter [env] is an evaluation environment, that is, a
   mapping of atoms to values, as defined above. It reflects the
   declarations that were successfully parsed, imported, and
   evaluated.

   The parameter [suspension] produces a stream of declarations in
   internal form or possibly fails with an [UnboundIdentifier]
   exception emanating from [Syntax.import_declarations]. Elements in
   the input stream that fail when evaluated are dropped in the output
   stream. *)

let rec process (env : env) (suspension : suspension) : esuspension =
  Suspension.create (fun () ->
    try
      (* Attempt to evaluate the head of the input stream. This
	 parses a line of input using [parse_declarations]
	 and imports it using [Syntax.import_declarations].
	 Execution of both of these functions gets suspended
	 again after the head of the stream has been obtained. *)
      match Suspension.force suspension with
      (* Success. Continue by examining the head of the stream,
	 which is now available. *)
      | Syntax.D d ->
	  (* Look at the first declaration in the stream. Open it
	     to get a fresh atom. *)
	  let x, e, new_suspension = Syntax.open_declaration d in
	  (* Attempt to evaluate it. *)
	  let v = eval env e in
	  (* Success. Produce a new output stream cell, whose tail is
	     obtained by processing the tail of the input stream in
	     an extended environment. *)
	  let env = Syntax.Var.AtomMap.add x v env in
	  Syntax.ED (Syntax.create_evaluated_declaration (
	    x, e, v, process env new_suspension
          ))
    with
    | Syntax.Var.UnboundIdentifier x ->
	(* Failure of [Syntax.import_declarations]. Print an error
	   message and continue by starting afresh with the same
	   parameters. This, in particular, has the effect of resuming
	   the execution of [Syntax.import_declarations] where it
	   left off, without extending its (to us, hidden) mapping
	   of identifiers to atoms.*)
	Printf.printf "Sorry -- the identifier \"%s\" is unbound.\n" x;
	flush stdout;
	Suspension.force (process env suspension)
    | EvaluationFailure ->
	(* Failure of [eval], handled similarly. *)
	Printf.printf "Sorry -- evaluation error.\n";
	flush stdout;
	Suspension.force (process env suspension)
  )

(* Printers for raw values, expressions, and evaluated declarations. *)

let print_value c = function
  | Syntax.Raw.VConst k ->
      Printf.fprintf c "%d" k

let rec print_expr c = function
  | Syntax.Raw.EVar x ->
      Printf.fprintf c "%s" x
  | Syntax.Raw.EConst k ->
      Printf.fprintf c "%d" k
  | Syntax.Raw.EAdd (e1, e2) ->
      (* Don't care about parentheses. *)
      Printf.fprintf c "%a + %a" print_expr e1 print_expr e2
  | Syntax.Raw.EFail ->
      Printf.fprintf c "fail"

let rec print_evaluated_declarations suspension =
  match Suspension.force suspension with
  | Syntax.Raw.ED (x, e, v, suspension) ->
      Printf.printf "%s is now bound to the value of %a, namely %a.\n" x print_expr e print_value v;
      flush stdout;
      print_evaluated_declarations suspension

(* Reverse application. *)

let (/) x f =
  f x

(* Use [parse_declarations] to obtain a stream of raw declarations.
   Pipe it into [Syntax.import_declarations] to obtain a stream of
   declarations in internal form. Pipe that into [process] to obtain a
   stream of evaluated declarations in internal form. Pipe that next
   into [Syntax.export_evaluated_declarations] to obtain a stream of
   evaluated declarations in raw form. Finally, print that stream.
   The whole setup is lazy. The printer plays the role of an eager
   consumer that drives the pipeline. *)

let () =
  try
    Suspension.create parse_declarations
  / Suspension.map (Syntax.import_declarations Syntax.Identifier.Map.empty)
  / process Syntax.Var.AtomMap.empty
  / Suspension.map (Syntax.export_evaluated_declarations Syntax.Var.AtomIdMap.empty)
  / print_evaluated_declarations
  with End_of_file ->
    ()

(* TEMPORARY

   We have a problem if the identifiers chosen at export do not coincide
   with the original identifiers typed in by the user. In that case, the
   reports provided by the printer can state that "x1 is now bound ...",
   whereas it is really "x" that is now bound. In other words, in the
   input term that is being parsed, it is really the original identifier
   that is bound, while in the output term that is being printed, another
   identifier has been chosen, and it is really that new identifier that
   is being bound. That is, we are reasoning with two different namespaces
   at once.

     # x = 3
     x is now bound to the value of 3, namely 3.
     # x = x + 1
     x1 is now bound to the value of x + 1, namely 4.
     # x = x + 1
     x2 is now bound to the value of x1 + 1, namely 5.

     # y3 = 0
     y is now bound to the value of 0, namely 0.

   It is not yet clear to me how to avoid this phenomenon. Checking that
   the user does not define the same name twice would pollute the code
   and would not be a sufficient condition, as illustrated by the case
   of "y3".

   It is also not clear to me whether this mind-boggling setup is at all
   a good idea.

*)

