(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/main.ml,v 1.61.2.30 1999/04/08 12:43:45 francois Exp $ *)
(*

Implements both the toplevel mode and the batch-mode typechecker.

*)

open Errors
open Types
open Interpreter
open Closure
open Connexity
open Flags
open Typechecking
open Printing
open Type_expr
open Small
open Equivalence
open Walk
open Env

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Global error handler.

*)

exception InvalidDirective of string
exception NonTrivialBinding of string

let print_exception = function

  (* Errors signaling "syntactically" incorrect input from the user. *)

  Lexer.Lexical_error message ->
    print_endline ("Lexical error: " ^ message)
    (* The lexer failed; the input cannot be converted to a stream of tokens. This is an uncommon error. *)

| Parsing.Parse_error ->
    print_endline "Parse error."
    (* The parser failed; the input is grammatically incorrect. Unfortunately, this is a common error! *)

| Type_expr.MalformedTypeExpression message ->
    print_endline ("Malformed type expression: " ^ message)
    (* A type scheme expression, although accepted by the parser, is incorrect. This mainly occurs when
       a certain label appears twice in a record or variant type. It also occurs when a context contains
       duplicate, unbound or let-bound names, but users are not expected to specify contexts in normal
       use. This is an uncommon error. *)

| Type_expr.IllKinded ->
    print_endline "Ill-kinded type expression."
    (* A type scheme expression, although grammatically correct, is rejected by the kind system. This
       indicates improper use of row variables, as in the following case:
         value x : { l : 'alpha; 'rho } -> { 'rho } raises 0;;
       where the function's return type contains no information about the label 'l'. *)

| Typechecking.BadSourceTerm message ->
    print_endline ("Ill-formed source term: " ^ message)
    (* This error indicates an incorrect source term. The term contains an unbound program variable,
       or an ill-formed record expression (where a certain label appears twice). *)

| Interpreter.InconsistentMatching message ->
    print_endline ("Ill-formed source pattern: " ^ message)
    (* This error indicates an incorrect pattern in a pattern matching. *)

| InvalidDirective directive ->
    print_endline ("Unknown directive " ^ directive)
    (* The user attempted to use an unknown directive #foo. *)

| Matching.MatchCompError message ->
    print_endline ("Invalid matching. " ^ message)
    (* The pattern matching compiler was unable to translate a complex pattern matching into a simple one.
       This is probably a structural error, such as not all patterns having the same arity. *)

| Abstract.Duplicate name ->
    print_endline ("Duplicate abstract type definition: " ^ name)
    (* An abstract type with this name already exists. Happens when trying to define a new type with an
       old name. *)

| Abstract.Unknown name ->
    print_endline ("Unknown abstract type: " ^ name)
    (* No abstract type by this name exists. Happens when referring to an unknown type in a type expression. *)

| Type_expr.ArityMismatch (name, expected, actual) ->
    print_string ("Abstract type " ^ name ^ " expects ");
    print_int expected;
    print_string " parameter(s), but is here used with ";
    print_int actual;
    print_endline " parameter(s)."
    (* Happens when a referring to an existing abstract type, but providing the wrong number of parameters. *)

  (* Errors signaling "semantically" incorrect input from the user. *)

| Inconsistent constraint_list ->
    print_endline "Inconsistent constraint:";
    List.iter (fun (ty1, ty2) ->
      print_constraint false ty1 ty2
    ) constraint_list
    (* This error occurs when an inconsistency in the subtyping constraints appears. The last constraint in the
       list is the one that caused the inconsistency, and the others are constraints that lead to it during the
       closure computation (by transitivity and structural decomposition). This implementation is not suitable
       for a real-world compiler; the data flow needs to be shown to the user by establishing a correspondence
       between type variables and program nodes. *)

| Subtyping.MissingContextEntry (name, _) ->
    print_endline ("Invalid cast: no context entry for " ^ name ^ " in supplied type scheme.")
    (* This error can occur when dealing with a cast construct, of the form (e : sigma) where e is an expression
       and sigma is a type scheme. If the inferred scheme for the expression has a non-empty context (i.e. if the
       expression contains free lambda-bound variables), then sigma should also have a context with (at least) as
       many entries. This is, of course, a rather undesirable feature of our type system, and we do not expect
       casual users to understand this error; a better system is needed here. Fortunately, most type casts should
       be done at toplevel, so expressions should be closed and no contexts should be needed. Hence, this should
       be an uncommon error. *)

| NonTrivialBinding name ->
    print_endline ("Variable " ^ name ^ " has non-trivial expansive definition.")
    (* A value has an expansive definition (i.e. its evaluation might create new reference cells), yet it has
       a polymorphic type. The definition must be rejected for safety of the type system. *)

  (* The following errors are not expected to happen in practice. *)

| Interpreter.ExecutionFailure msg ->
    print_endline ("Execution failure: " ^ msg)
    (* This error signals an execution failure by the interpreter, i.e. it means that the type system has a hole
       and has accepted an unsafe program. However, there are a few circumstances where this error is to be
       expected. It can happen if a value is missing from the execution environment (because it was declared
       by a value declaration, which does not require an implementation to be supplied). It can also happen when
       executing certain well-typed recursive definitions, such as
         let rec x = (x, x);;
       The interpreter is too naive and improperly executes these definitions. There is no safeguard against this
       problem. *)

| Interpreter.WrongMatchStructure ->
    print_endline "Execution failure: wrong match structure."
    (* A pattern does not have the same structure as the value to be filtered. This signals a hole in the type
       system. *)

| Interpreter.MatchFailure ->
    print_endline ("Match failure.")
    (* A value matches none of the patterns in a matching. This signals a hole in the type system, since pattern
       matchings are always exhaustive (i.e. they receive conservative types). *)

| CantHappen message ->
    print_endline "Can't Happen Error (congrats ;-):";
    print_endline message
    (* An internal invariant has been broken! The author did not expect this code path to be ever followed. *)

  (* The following errors can occur as part of the program's normal behavior. *)

| Interpreter.UserException value ->
    print_string "Uncaught user exception: ";
    print_value value;
    print_endline ""
    (* The user program's evaluation has been aborted by a user exception. *)

| Sys_error message ->
    print_endline ("System error: " ^ message)
    (* Some system call failed; for instance, the user attempted to include a non-existent file. *)

  (* Unknown exceptions (such as those raised by O'Caml's runtime system) are ignored. *)

| other ->
    raise other
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Checking whether a type scheme is trivial. 

A type scheme is trivial if it allows no polymorphism at all. Intuitively, this means that its type variables
are used only as node labels and do not add any power to it. (See my PhD thesis for details.) If we drop the
small terms restriction, a trivial type scheme can be printed without using any type variables (except mu-bounded
variables, if the scheme contains recursive constraints).

If a definition looks expansive to the typechecker (i.e. its evaluation might create new reference cells), then
its type must not be polymorphic, for the type system to be safe. Hence, expansive definitions must receive trivial
schemes. Otherwise, an error occurs. Here, we temporarily issue a warning instead of an error, because this allows
us to accept more existing code; it is, however, incorrect.

*)

let trivial_binding name scheme =
  try
    Walk.iter_scheme (fun v ->
		      	if not ((Set7.is_empty v.loset) & (Set7.is_empty v.hiset)) then
			  raise (NonTrivialBinding name)
    ) scheme
  with NonTrivialBinding name ->
    print_endline ("Warning: variable " ^ name ^ " has non-trivial expansive definition.")
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Initializing the global environments.

Note that we don't deal with effects at all at toplevel, since exceptions generated by evaluating a toplevel
expression are never caught. This can be misleading to the user, since the typechecker seems to report that no
exceptions can be raised by the toplevel expression. One has to keep in mind that the typechecker prints the new
environment entry; and no exceptions can be raised by accessing the environment, so it is correct.

*)

let type_environment = ref Builtin.type_environment
and exec_environment = ref Builtin.exec_environment

let infer_clock = Chrono.create()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling directives.

#dump writes the whole type environment to a file.
#reset resets all internal timers.
#<flag> toggles a boolean flag (see Flags).

*)

let rec switch name = function
  [] ->
    raise (InvalidDirective name)
| (flagname, flag) :: rest ->
    if flagname = name then begin
      flag := not !flag;
      if !Flags.filename = None then begin
        print_endline (name ^ " flag turned " ^ (if !flag then "on" else "off"));
	print_newline()
      end
    end
    else switch name rest
;;

let handle_directive directive =
  if directive = "dump" then
    Frozen.dump !type_environment
  else if directive = "reset" then begin
    Chrono.reset infer_clock;
    Chrono.reset normalize_clock;
    Chrono.reset canonize_clock;
    Chrono.reset garbage_polarity_clock;
    Chrono.reset garbage_collection_clock;
    Chrono.reset reduction_clock;
    Chrono.reset simplify_clock
  end
  else
    switch directive Flags.flags
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Printing a type scheme, raw and beautified. This function is called only in interactive mode.

*)

let detailed_print_scheme name scheme =

  Connexity.polarity scheme;

  (* Print the simplified scheme. *)

  print_endline ("Variable " ^ name ^ " defined:");
  print_type_scheme true true scheme;
  print_newline();

  (* Re-establish polarities, which have been modified by the pretty-printer. *)

  Connexity.polarity scheme;

  (* If in verbose mode, print more information. *)

  if !Flags.verbose then begin

    print_endline "Internal representation:";
    print_type_scheme true false scheme;
    print_newline()

  end
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling value declarations.

*)

let handle_phrase_value name scheme_expr =

  let scheme = Chrono.chrono infer_clock (fun () ->

    (* Read the type scheme. *)

    let scheme = scheme_expression_to_scheme !type_environment scheme_expr in

    (* Simplify it (including bipolar variable elimination) and enter it into the environment. *)

    simplify true LetNode scheme

  ) in

  type_environment := (Let(name, scheme)) :: !type_environment;

  if ((!Flags.filename = None) & !Flags.exec) then
    exec_environment := (name, ref ValUninitialized) :: !exec_environment;

  (* Acknowledge if necessary. *)

  if !Flags.filename = None then
    detailed_print_scheme name scheme
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling abstract type declarations.

*)

let handle_abstract_type name variance =
  let _ = Abstract.create name variance in
  if !Flags.filename = None then
    print_endline ("Abstract type " ^ name ^ " defined.");
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling toplevel let definitions. Note that (unnamed) toplevel expressions are transformed into let definitions
by the parser, for the sake of simplicity.

*)

let handle_toplevel_let recursive expansive bindings =

  (* Typecheck the bindings. *)

  if !Flags.filename = None then
    handle_directive "reset";

  let new_type_chunk = Chrono.chrono infer_clock (fun () ->

    (* Infer a new environment chunk (note that the context and effect are dropped). *)

    let chunk, _, _ = create_let_bindings LetNode recursive !type_environment bindings in

    (* If the bindings are expansive, we cannot allow them to be generalized. The bindings should be rejected
       unless they are trivial. *)

    if expansive then
      do_let_env trivial_binding chunk;
    chunk

  ) in

  if (!Flags.filename = None) & !Flags.verbose then
    Chrono.print infer_clock "Typechecking" false;

  (* Print the results and update our score after beautification. This process is not timed, because it is not
     necessary when running in batch mode. *)

  if !Flags.filename = None then
    do_let_env detailed_print_scheme new_type_chunk;

  (* Update the type environment. This must be done prior to execution, since execution might raise a
     (user) exception. *)

  type_environment := new_type_chunk @ !type_environment;

  (* In interactive mode, execute the let bindings. *)

  if ((!Flags.filename = None) & !Flags.exec) then begin

    print_string "\nExecuting... "; flush stdout;
    let new_exec_environment_chunk = create_let_exec_environment recursive !exec_environment bindings in

    (* Print the results. *)

    List.iter (function (name, value) ->
      print_string (name ^ " = ");
      print_value !value;
      print_endline "";
      flush stdout
    ) new_exec_environment_chunk;

    (* Update the global execution environment. *)

    exec_environment := new_exec_environment_chunk @ !exec_environment

  end
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main control loop.

*)

let rec handle_phrase = function
    PhraseDirective directive ->
      handle_directive directive
  | PhraseLet (recursive, bindings) ->
      let expansive, bindings = Wright.filter_bindings bindings in
      let bindings = Matching.compile_bindings bindings in
      if !Flags.showcode then begin
        print_endline "The code output by the pattern matching compiler is";
        Interpreter.print_toplevel_let_bindings recursive bindings;
        print_endline ""
      end;
      handle_toplevel_let recursive expansive bindings
  | PhraseValue (name, scheme_expr) ->
      handle_phrase_value name scheme_expr
  | PhraseInclude filename ->
      handle_channel false filename (open_in filename)
  | PhraseAbstractType (name, variance) ->
      handle_abstract_type name variance

and handle_channel is_main_module filename channel =
  let lexbuf = Lexing.from_channel channel in
  try
    while true do
      try

	if !Flags.filename = None then begin
	  Printing.reset();
	  print_string "?"; flush stdout
	end;

	let phrase = Parser.phrase Lexer.token lexbuf in

	if !Flags.filename = None then
	  print_newline();

	handle_phrase phrase

      with
	End_of_file ->
	  raise End_of_file
      | error ->
	  print_exception error;
	  if !Flags.filename <> None then
	    exit(1)
    done
  with End_of_file ->
    if (!Flags.filename <> None) & !Flags.verbose then begin

      print_endline ("Done typechecking " ^ filename ^ ".");
      Chrono.print infer_clock (if is_main_module then "Typechecking" else "So far, typechecking") false;

      let generation_and_closure_clock = Chrono.diff infer_clock simplify_clock in
      Chrono.print generation_and_closure_clock "Constraint generation and closure" false;
      Chrono.print simplify_clock "Simplification" false;

      print_endline "In particular:";
      Chrono.print normalize_clock "Normalization" true;
      Chrono.print canonize_clock "Canonization" true;
      Chrono.print garbage_polarity_clock "Computing polarities" true;
      Chrono.print garbage_collection_clock "Actual garbage collection" true;
      Chrono.print reduction_clock "Minimization" true

    end
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Start-up code.

*)

Chrono.start();; (* By starting Chrono here, we don't take simplifications made by Builtin into account. *)

if not !Sys.interactive then begin
  match !Flags.filename with
    None ->
      handle_channel false "" stdin
  | Some filename ->
      handle_channel true filename (open_in filename)
end
;;
