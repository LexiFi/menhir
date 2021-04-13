(* ------------------------------------------------------------------------ *)
(* Here is a bunch of naming conventions. Our names are chosen to minimize
   the likelihood that a name in a semantic action is captured. In other
   words, all global definitions as well as the parameters to [reduce]
   are given far-fetched names, unless [--no-prefix] was specified. Note
   that the prefix must begin with '_'. This allows avoiding warnings
   about unused variables with ocaml 3.09 and later. *)

let prefix name = if Settings.noprefix then name else "_menhir_" ^ name

(** A variable used to hold a semantic value. *)
let semv = "_v"

(** A variable used to hold a stack. *)
let stack = prefix "stack"

(** A variable used to hold a state. *)
let state = prefix "s"

(** A variable used to hold a token. *)
let token = "_tok"

(* Variables used to hold start and end positions. Do not change these
   names! They are chosen to coincide with the $startpos and $endpos
   keywords, which the lexer rewrites to _startpos and _endpos, so
   that binding these variables before executing a semantic action is
   meaningful. *)

(* These names should agree with the printing function [Keyword.posvar]. *)

(** "_endpos__0_" *)
let beforeendp = Keyword.(posvar Before WhereEnd FlavorPosition)

(** "_startpos" *)
let startp = Keyword.(posvar Left WhereStart FlavorPosition)

(** "_endpos" *)
let endp = Keyword.(posvar Left WhereEnd FlavorPosition)

(** sprintf "_startpos_%s_" ids.(i) *)
let startpos ids i =
  Keyword.(posvar (RightNamed ids.(i)) WhereStart FlavorPosition)

(** sprintf "_endpos_%s_" ids.(i) *)
let endpos ids i = Keyword.(posvar (RightNamed ids.(i)) WhereEnd FlavorPosition)

let lexer = prefix "lexer"

let lexbuf = prefix "lexbuf"
