open Syntax

type producer =
  {
    producer_identifier : identifier;
    producer_symbol     : symbol;
    producer_attributes : attributes;
  }

type producers =
  producer list

type branch =
    {
      branch_position           : Positions.t;
      producers                 : producers;
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
      attributes           : attributes;
    }

type grammar =
    {
      preludes             : Stretch.t list;
      postludes            : Syntax.postlude list;
      parameters           : Stretch.t list;
      start_symbols        : StringSet.t;
      types                : Stretch.ocamltype StringMap.t;
      on_error_reduce      : on_error_reduce_level StringMap.t;
      tokens               : Syntax.token_properties StringMap.t;
      gr_attributes        : attributes;
      rules                : rule StringMap.t;
    }

(* Accessors for the type [producer]. *)

let producer_identifier { producer_identifier } = producer_identifier
let producer_symbol { producer_symbol } = producer_symbol
let producer_attributes { producer_attributes } = producer_attributes

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
