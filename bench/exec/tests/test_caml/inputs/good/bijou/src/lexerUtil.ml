open Lexing
open Tokens

(* [newline lexbuf] increments the line counter stored within [lexbuf]. *)

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

(* This sets up bidirectional tables that translate between strings
   and (unparameterized) tokens. *)

let setup sts =
  let direct = Hashtbl.create 123
  and reverse = Hashtbl.create 123 in
  List.iter (fun (s, t) ->
    Hashtbl.add direct s t;
    Hashtbl.add reverse t s
  ) sts;
  Hashtbl.find direct, Hashtbl.find reverse

(* A table of keywords. *)

let string2keyword, keyword2string =
  setup [
    "empty", EMPTYSET;
    "closed", CLOSED;
    "end", END;
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "where", WHERE;
    "and", BAND;
    "nothing", NOTHING;
    "type", TYPE;
    "inner", INNER;
    "outer", OUTER;
    "free", SUPPORT;
    "bound", BOUND;
    "binds", BINDS;
    "atom", ATOM;
    "atom_set", ATOMSET;
    "bool", BOOL;
    "true", TRUE;
    "false", FALSE;
    "or", BOR;
    "not", BNOT;
    "sort", SORT;
    "lemma", LEMMA;
    "forall", FORALL;
    "fresh", FRESH;
    "let", LET;
    "assert", ASSERT;
    "check", CHECK;
    "fun", FUN;
    "case", CASE;
    "of", OF;
    "in", IN;
    "raise", RAISE;
    "unless", UNLESS;
    "fail", FAIL;
    "absurd", ABSURD;
    "exception", EXCEPTION;
    "try", TRY;
  ]

(* A table of built-in identifiers. *)

let string2builtin, builtin2string =
  setup [
    "==", CMPEQ;
  ]

let builtin2id t =
  Location.none (builtin2string t)

