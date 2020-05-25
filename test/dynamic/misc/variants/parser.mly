%token T
%token EOF

(* This parser is tricky. The semantic action returns [> `T of int], but the
   start symbol is declared to have type [> `A of string], so the real type of
   the start symbol is [> `A of string | `T of int]. *)

(* Versions of Menhir prior to 2020/05/25 would mistakenly assume that the
   start symbol has type [> `A of string]. This could be exploited by the
   user to cause a segmentation fault. *)

(* This issue was reported by Joe. *)

%start < [> `A of string ] > main

%%

main:
  | T EOF { `T 0 }
