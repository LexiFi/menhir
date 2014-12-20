(* A parameterized branch may instantiate parameterized non terminals. 
   If the parameterized branch contributes to the definition of a 
   parameterized terminal, then the instantiation of parameterized 
   non terminals that are defined simultaneously must only be done with
   formal parameters.
   Furthermore, all the parameterized non terminals that are in a common
   mutual recursive definition must have the same arity.
   These conditions are sufficient to ensure termination of expansion.
   For example:
   C[x] : ...            // This definition does not involve A or B.
   A[x,y] : B[x,y] C[Y]  // This mutual recursive definition is ok.
   B[x,y] : A[x,y]
   D[x] : E[D[x]]        // This one is incorrect.
   E[y] : D[y]
*)
open Syntax

type branch =
    {
      branch_position           : Positions.t;
      producers			: (symbol * identifier option) list; (* TEMPORARY convention renversée
                                    par rapport à syntax.mli; faire un type record au lieu d'une paire? *)
      action			: action;
      branch_shift_precedence	: branch_shift_precedence;
      branch_reduce_precedence  : branch_reduce_precedence
    }

type rule = 
    {
      branches		   : branch list;
      positions            : Positions.t list;
      (* This flag is not relevant after the NonTerminalInlining.inline pass. *)
      inline_flag          : bool;
    }

type grammar = 
    {
      preludes	           : Stretch.t list;
      postludes	           : Syntax.trailer list;
      parameters           : Stretch.t list;
      start_symbols        : StringSet.t;
      types                : Stretch.ocamltype StringMap.t;
      tokens	           : Syntax.token_properties StringMap.t;
      rules	           : rule StringMap.t;
    }

let nonterminals grammar =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []
