(* The type [partial_grammar] describes the abstract syntax that is produced
   by the parsers (yacc-parser and fancy-parser).

   The type [grammar] describes the abstract syntax that is obtained after one
   or more partial grammars are joined (see [PartialGrammar]). It differs in
   that declarations are organized in a more useful way and a number of
   well-formedness checks have been performed. *)

(* ------------------------------------------------------------------------ *)

(* Terminals and nonterminal symbols are strings. Identifiers
   (which are used to refer to a symbol's semantic value) are
   strings. A file name is a string. *)

type terminal =
    string

type nonterminal =
    string

type symbol =
    string

type identifier =
    string

type filename =
    string

(* ------------------------------------------------------------------------ *)

(* A trailer is a source file fragment. *)

type trailer =
    Stretch.t

(* ------------------------------------------------------------------------ *)

(* OCaml semantic actions are represented as stretches. *)

type action =
    Action.t

(* ------------------------------------------------------------------------ *)

(* Information about tokens. (Only after joining.) *)

type token_associativity =
    LeftAssoc
  | RightAssoc
  | NonAssoc
  | UndefinedAssoc

type precedence_level =
    UndefinedPrecedence

  (* Items are incomparable when they originate in different files. A
     brand of type [Mark.t] is used to record an item's origin. The
     positions allow locating certain warnings. *)

  | PrecedenceLevel of Mark.t * int * Lexing.position * Lexing.position

type token_properties =
    {
               tk_filename      : filename;
               tk_ocamltype     : Stretch.ocamltype option;
               tk_position      : Positions.t;
      mutable  tk_associativity : token_associativity;
      mutable  tk_precedence    : precedence_level;
      mutable  tk_is_declared   : bool;
    }

(* ------------------------------------------------------------------------ *)

(* A parameter is either just a symbol, or an application of a symbol to
   a tuple of parameters. *)

type parameter =
  | ParameterVar of symbol Positions.located
  | ParameterApp of symbol Positions.located * parameters

and parameters =
    parameter list

(* ------------------------------------------------------------------------ *)

(* A declaration. (Only before joining.) *)

type declaration =

    (* Raw OCaml code. *)

  | DCode of Stretch.t

    (* Raw OCaml functor parameter. *)

  | DParameter of Stretch.ocamltype (* really a stretch *)

    (* Terminal symbol (token) declaration. *)

  | DToken of Stretch.ocamltype option * terminal

    (* Start symbol declaration. *)

  | DStart of nonterminal

    (* Priority and associativity declaration. *)

  | DTokenProperties of terminal * token_associativity * precedence_level

    (* Type declaration. *)

  | DType of Stretch.ocamltype * parameter

    (* On-error-reduce declaration. *)

  | DOnErrorReduce of parameter

(* ------------------------------------------------------------------------ *)

(* A [%prec] annotation is optional. A production can carry at most one.
   If there is one, it is a symbol name. See [ParserAux]. *)

type branch_prec_annotation =
    symbol Positions.located option

(* ------------------------------------------------------------------------ *)

(* A "production level" is used to solve reduce/reduce conflicts. It reflects
   which production appears first in the grammar. See [ParserAux]. *)

type branch_production_level =
  | ProductionLevel of Mark.t * int

(* ------------------------------------------------------------------------ *)

(* A producer is a pair of identifier and a parameter. In concrete syntax,
   it could be [e = expr], for instance. *)

type producer =
    identifier Positions.located * parameter

(* ------------------------------------------------------------------------ *)

(* A branch contains a series of producers and a semantic action. *)

type parameterized_branch =
    {
      pr_branch_position           : Positions.t;
      pr_producers                 : producer list;
      pr_action                    : action;
      pr_branch_prec_annotation    : branch_prec_annotation;
      pr_branch_production_level   : branch_production_level
    }

(* ------------------------------------------------------------------------ *)

(* A rule has a header and several branches. *)

type parameterized_rule =
    {
      pr_public_flag       : bool;
      pr_inline_flag       : bool;
      pr_nt                : nonterminal;
      pr_positions         : Positions.t list;
      pr_parameters        : symbol list;
      pr_branches          : parameterized_branch list;
    }

(* ------------------------------------------------------------------------ *)

(* A partial grammar. (Only before joining.) *)

type partial_grammar =
    {
      pg_filename          : filename;
      pg_trailer           : trailer option;
      pg_declarations      : declaration Positions.located list;
      pg_rules             : parameterized_rule list;
    }

(* ------------------------------------------------------------------------ *)

(* A grammar. (Only after joining.) *)

(* The differences with partial grammars (above) are as follows:
   1. the file name is gone (there could be several file names, anyway).
   2. there can be several trailers, now known as postludes.
   3. declarations are organized by kind: preludes, functor %parameters,
      %start symbols, %types, %tokens, %on_error_reduce.
   4. rules are stored in a map, indexed by symbol names, instead of a list.
 *)

type grammar =
    {
      p_preludes           : Stretch.t list;
      p_postludes          : trailer list;
      p_parameters         : Stretch.t list;
      p_start_symbols      : Positions.t StringMap.t;
      p_types              : (parameter * Stretch.ocamltype Positions.located) list;
      p_tokens             : token_properties StringMap.t;
      p_on_error_reduce    : parameter list;
      p_rules              : parameterized_rule StringMap.t;
    }
