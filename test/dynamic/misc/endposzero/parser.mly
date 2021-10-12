%token A B C
%start<int> main

%%

main: A foo C
  { $2 }

foo: nothing B
  { $1 }
  (* Because [nothing] is inlined, this is equivalent to
     foo: B { $endofs($0) }
     The point is that we have a use of $endofs($0) in a
     production whose right-hand side has length greater
     than zero. This case is incorrectly treated by the
     code back-end up to 2021/10/12. *)
  (* The expected value of $endofs($0) here is the end
     offset of the previous token, which is A, so the
     expected value is 1. The incorrect code back-end
     produces 0 (because it reads a non-existent stack
     cell and luckily hits the cell below it), but one
     can probably craft a more complex example where
     it incorrectly casts a semantic value into an
     integer offset!. *)

%inline nothing:
  { $startofs }
