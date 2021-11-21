%token T
%token EOF

(* The new code back-end does not allow the type of a start symbol to be an
   open variant type such as [> `A of string | `T of int]. Indeed, this type
   contains an implicit type variable, but the manner in which the back-end
   boxes the final types requires them to be closed. So, we must work around
   the problem by declaring the type of the start symbol to be a closed type
   [ `A of string | `T of int]. *)

%start < [ `A of string | `T of int ] > main

%%

main:
  | T EOF { `T 0 }
