(* In this grammar, conflict resolution makes certain states inaccessible. *)

(* See issue 64 and merge request 29.
   https://gitlab.inria.fr/fpottier/menhir/-/merge_requests/29 *)

%token IDENT EOF
%start<unit> prog
%%
prog: decl* EOF { () }
decl: term { () }
term:
| IDENT { () }
| term term { () }
