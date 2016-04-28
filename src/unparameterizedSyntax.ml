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
      producers                 : (symbol * identifier) list; (* TEMPORARY convention renversée
                                    par rapport à syntax.mli; faire un type record au lieu d'une paire? *)
      action                    : action;
      branch_prec_annotation    : branch_prec_annotation;
      branch_production_level   : branch_production_level
    }

type rule =
    {
      branches             : branch list;
      positions            : Positions.t list;
      (* This flag is not relevant after the NonTerminalInlining.inline pass. *)
      inline_flag          : bool;
    }

type grammar =
    {
      preludes             : Stretch.t list;
      postludes            : Syntax.trailer list;
      parameters           : Stretch.t list;
      start_symbols        : StringSet.t;
      types                : Stretch.ocamltype StringMap.t;
      on_error_reduce      : StringSet.t;
      tokens               : Syntax.token_properties StringMap.t;
      rules                : rule StringMap.t;
    }

(* [tokens grammar] is a list of all (real) tokens in the grammar
   [grammar]. The special tokens "#" and "error" are not included.
   Pseudo-tokens (used in %prec declarations, but never declared
   using %token) are filtered out. *)

let tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then token :: tokens else tokens
  ) grammar.tokens []

(* [typed_tokens grammar] is analogous, but includes the OCaml type
   of each token. *)

let typed_tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then (token, properties.tk_ocamltype) :: tokens else tokens
  ) grammar.tokens []

(* [nonterminals grammar] is a list of all nonterminal symbols in the
   grammar [grammar]. *)

let nonterminals grammar : nonterminal list =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []

(* [ocamltype_of_symbol grammar symbol] produces the OCaml type
   of the symbol [symbol] in the grammar [grammar], if it is known. *)

let ocamltype_of_symbol grammar symbol : Stretch.ocamltype option =
  try
    Some (StringMap.find symbol grammar.types)
  with Not_found ->
    None

(* [ocamltype_of_start_symbol grammar symbol] produces the OCaml type
   of the start symbol [symbol] in the grammar [grammar]. *)

let ocamltype_of_start_symbol grammar symbol : Stretch.ocamltype =
  try
    StringMap.find symbol grammar.types
  with Not_found ->
    (* Every start symbol should have a type. *)
    assert false

