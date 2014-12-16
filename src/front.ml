(* Start where [PreFront] left off. *)

let grammar =
  PreFront.grammar

(* If [--only-tokens] was specified on the command line, produce
   the definition of the [token] type and stop. *)

let () =
  TokenType.produce_tokentype grammar

(* Perform reachability analysis. *)

let grammar =
  Reachability.trim grammar

let () =
  Time.tick "Trimming"

(* If [--depend] was specified on the command line, perform
   dependency analysis and stop. *)

let () =
  match Settings.depend with
  | Settings.OMRaw
  | Settings.OMPostprocess ->
      Infer.depend grammar (* never returns *)
  | Settings.OMNone ->
      ()

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal is then known. *)

let grammar =
  if Settings.infer then
    let grammar = Infer.infer grammar in
    Time.tick "Inferring types for nonterminals";
    grammar
  else
    grammar

(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

let grammar =
  if Settings.inline then begin
    let grammar, inlined = 
      NonTerminalDefinitionInlining.inline grammar
    in
    if not Settings.infer && inlined && not Settings.coq then
      Error.warning []
	"you are using the standard library and/or the %inline keyword. We\n\
	 recommend switching on --infer in order to avoid obscure type error messages.";
    Time.tick "Inlining";
    grammar
  end
  else 
    grammar

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)

let () =
  match Settings.preprocess_mode with
  | Settings.PMOnlyPreprocess mode ->
      UnparameterizedPrinter.print mode stdout grammar;
      exit 0
  | Settings.PMNormal ->
      ()

