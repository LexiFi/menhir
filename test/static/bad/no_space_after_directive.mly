%tokenA
%start<unit> foo
  (* This was accepted until 2020/06/06! *)

%%

foo:
  A {}
