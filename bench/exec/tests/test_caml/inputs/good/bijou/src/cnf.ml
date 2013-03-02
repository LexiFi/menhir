open Printf

(* ------------------------------------------------------------------------- *)

(* Boolean variables. *)

module Var : AlphaLib.Signatures.Atom with type identifier = string =
    AlphaLib.Atom.Make(AlphaLib.Atom.String)

type variable =
    Var.Atom.t

module VariableSet =
  Var.AtomSet

module VariableMap =
  Var.AtomMap

(* ------------------------------------------------------------------------- *)

(* Problems in conjunctive normal form. *)

(* A clause is a disjunction of literals. It is represented as a set
   of positive literals together with a set of negative ones. No
   variable appears in both sets. *)

(* We do not allow a clause to be empty, even though that would make
   sense (an empty clause is false), because zChaff does not seem to
   understand that empty clauses are false. It views them as true. *)

type clause = {
    positive: VariableSet.t;
    negative: VariableSet.t
  }

module ClauseSet = Set.Make (struct
  type t = clause
  let compare
    { positive = positive1; negative = negative1 }
    { positive = positive2; negative = negative2 }
      =
    Order.lexicographic2
      VariableSet.compare
      VariableSet.compare 
      positive1 negative1
      positive2 negative2

end)

(* A problem is either false or a conjunction of clauses. *)

type problem =
  | PFalse
  | PConj of ClauseSet.t

(* ------------------------------------------------------------------------- *)

(* Building up clauses. *)

let positive v =
  {
    positive = VariableSet.singleton v;
    negative = VariableSet.empty
  }

let negative v =
  {
    positive = VariableSet.empty;
    negative = VariableSet.singleton v
  }

let disjunction
  { positive = positive1; negative = negative1 }
  { positive = positive2; negative = negative2 }
    =
  let positive = VariableSet.union positive1 positive2
  and negative = VariableSet.union negative1 negative2 in
  if VariableSet.is_empty (VariableSet.inter positive negative) then
    Some { positive = positive; negative = negative }
  else
    None (* clause has become a tautology, drop it *)

let falsity =
  PFalse

let truth =
  PConj ClauseSet.empty

let singleton clause =
  PConj (ClauseSet.singleton clause)

let conjunction problem1 problem2 =
  match problem1, problem2 with
  | PFalse, _
  | _, PFalse ->
      PFalse
  | PConj clauses1, PConj clauses2 ->
      PConj (ClauseSet.union clauses1 clauses2)

let disjunction problem1 problem2 =
  match problem1, problem2 with
  | PFalse, problem
  | problem, PFalse ->
      problem
  | PConj clauses1, PConj clauses2 ->
      PConj (
	ClauseSet.fold (fun clause1 clauses ->
	  ClauseSet.fold (fun clause2 clauses ->
	    match disjunction clause1 clause2 with
	    | None ->
		clauses
	    | Some clause ->
		ClauseSet.add clause clauses
	  ) clauses2 clauses
	) clauses1 ClauseSet.empty
      )

(* ------------------------------------------------------------------------- *)

(* Display in DIMACS CNF format. *)

let buffer =
  Buffer.create 32768

module N =
  Number.Make(Var)

let print_clauses channel print comment clauses =

  (* Gather all variables. Count the clauses. *)

  let variables, ccount =
    ClauseSet.fold (fun { positive = positive; negative = negative } (variables, ccount) ->
      VariableSet.union positive (VariableSet.union negative variables),
      ccount + 1
    ) clauses (VariableSet.empty, 0)
  in

  (* Number the variables from [1] up. *)

  let vcount, direct, reverse = N.number 1 variables in

  (* Define printers. *)

  let print_variable numeric negated v =
    let print =
      if numeric then
	fun b v -> bprintf b "%d" (direct v)
      else
	print
    in
    bprintf buffer "%s%a " (if negated then "-" else "") print v
  in

  let print_clause numeric { positive = positive; negative = negative } =
    if not numeric then Buffer.add_string buffer "c ";
    VariableSet.iter (print_variable numeric true) negative;
    VariableSet.iter (print_variable numeric false) positive;
    Buffer.add_string buffer (if numeric then "0\n" else "\n")
  in

  (* Clear the (global) buffer. *)

  Buffer.clear buffer;

  (* Print the comment. *)

  Buffer.add_string buffer (Tack.tack (Lexing.from_string comment));

  (* Print the header. *)

  bprintf buffer "p cnf %d %d\n" vcount ccount;

  (* Print all clauses in human-readable format. *)

  if false then (* toggle *)
    ClauseSet.iter (print_clause false) clauses;

  (* Print a variable name table. *)

  if false then (* toggle *)
    for i = 1 to vcount do
      bprintf buffer "c %3d stands for %a\n" i print (reverse i)
    done;

  (* Print all clauses in numeric format. *)

  ClauseSet.iter (print_clause true) clauses;

  (* Finish. *)

  Buffer.output_buffer channel buffer;

  (* Construct and return a function for converting the solution back. *)

  let convert (vs : int list) =
    let set =
      List.fold_left (fun accu v ->
	VariableSet.add (reverse v) accu
      ) VariableSet.empty vs
    in
    VariableSet.fold (fun v accu ->
      VariableMap.add v (VariableSet.mem v set) accu
    ) variables VariableMap.empty
  in

  convert

(* ------------------------------------------------------------------------- *)

(* Export to an external SAT solver for solving. At the moment, two solvers
   are supported: zChaff and relsat. *)

type solution =
    bool Var.AtomMap.t

type solver =
  | SolverZChaff
  | SolverRelsat

type mode =
  | ModeOneSolution
  | ModeAllSolutions

let command solver mode =
  match solver, mode with
  | SolverZChaff, ModeOneSolution ->
      "zchaff"
  | SolverZChaff, ModeAllSolutions ->
      assert false (* unimplemented *)
  | SolverRelsat, ModeOneSolution ->
      "relsat -#1"
  | SolverRelsat, ModeAllSolutions ->
      "relsat -#a"

let lexer = function
  | SolverZChaff ->
      ZChaff.main
  | SolverRelsat ->
      Relsat.main

let solver =
  ref SolverRelsat

let count =
  ref 0

let solve print comment mode = function
  | PFalse ->
      []
  | PConj clauses when ClauseSet.is_empty clauses ->
      [ Var.AtomMap.empty ] (* works around a stupid bug in relsat *)
  | PConj clauses ->

      (* Open a temporary file to hold the problem statement. *)

      let filename, channel =
	Filename.open_temp_file "fml" ".cnf"
      in

      (* Write the problem statement. *)

      let convert = print_clauses channel print comment clauses in
      close_out channel;

      (* Invoke the external command. *)

      incr count;

      let command =
	sprintf "%s %s" (command !solver mode) (String.escaped filename)
      in

      match IO.invoke command with
      | None ->

	  (* Presumably, the command printed an error message for us.
	     But one never knows, so let's display our own message.
	     We do not remove the file in that case, so it can be
	     inspected. *)

	  Error.error [] (sprintf "Error: external command failed: %s\n%!" command)

      | Some result ->

	  (* Remove the file that contains the problem statement. *)

	  Sys.remove filename;

	  (* Read the solutions back. *)

	  let solutions =
	    try
	      (lexer !solver) (Lexing.from_string result)
	    with
	    | ZChaff.Failure
	    | Relsat.Failure ->
		Error.error [] (sprintf "Internal error (cnf): could not lex:\n%s" result)
	  in

	  List.map convert solutions

(* ------------------------------------------------------------------------- *)

(* Statistics. *)

let stats () =
  fprintf stderr "The SAT solver was invoked %d times.\n%!"
    !count

