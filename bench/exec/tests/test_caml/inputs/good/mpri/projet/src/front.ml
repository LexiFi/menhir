open Printf
open AbstractSyntax

(* Auxiliary function to type-check a program *)

let type_program (Program(_, main) as program) =
  (* Check that the program is well-formed and extract the data constructor
     table. *)
  let dcenv =
    Wf.check_program program in
  (* Perform type inference *)
  let module P = Generator.Run (struct
    let dcenv = dcenv
    let main = main
  end) in
  try
    let mgu = Unification.mgu P.problem in
    try
      Unification.Var.Map.find P.root mgu
    with Not_found ->
      assert false
  with
  | Unification.Mismatch ->
      Error.error [] "Unification error (mismatch)."
  | Unification.OccurCheck ->
      Error.error [] "Unification error (occur check)."

(* ------------------------------------------------------------------------- *)

(* Read the file and parse the program. *)

let Program (_, main) as program =
  let channel = open_in Settings.filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <-
      { 
	Lexing.pos_fname = Settings.filename; 
	Lexing.pos_lnum  = 1;
	Lexing.pos_bol   = 0; 
	Lexing.pos_cnum  = 0
      };
  try
    let program = Parser.program Lexer.main lexbuf in
    close_in channel;
    program
  with Parser.Error ->
    Error.error
      [ Some (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) ]
      "Syntax error."

(* ------------------------------------------------------------------------- *)

(* Perform type inference and print the program's type. *)

let _ =
  let t = type_program program in
  printf "*** Type:\n";
  Unification.print Format.std_formatter t;
  Format.printf "@.\n"

(* ------------------------------------------------------------------------- *)

(* Execute the program *)

let _ =
  printf "*** Interpretation:\n";
  Interpreter.eval_program program

(* ------------------------------------------------------------------------- *)

(* Perform monadic translation, type-check and execute the modified program *)

let tprogram = Exnconv.transl_program program 

let _ =
  printf "*** The program after exception conversion:\n";
  PrintAbstractSyntax.print_program tprogram

(* Comment this out if your translation does not preserve types *)

let _ =
  let t = type_program tprogram in
  printf "*** Type after exception conversion:\n";
  Unification.print Format.std_formatter t;
  Format.printf "@.\n"

let _ =
  printf "*** Interpretation after exception conversion:\n";
  Interpreter.eval_program tprogram

(* ------------------------------------------------------------------------- *)

(* Compile the program, print the generated code, and execute it *)

let _ =
  try
    let code = Compiler.compile_program tprogram in
    printf "*** Compiled code:\n";
    Machine.print_program code;
    printf "*** Execution of the compiled code:\n";
    Machine.execute_program code
  with Compiler.CompilationError msg ->
    printf "*** Compilation error: %s\n" msg






