%token A B C
%start<unit> main
%%
(* Case: token in front. *)
main: A B hop { $symbolstartpos, $symbolstartofs }
(* Case: non-nullable symbol in front. *)
hop: bar A B { $symbolstartpos }
(* Case: nullable symbol in front. *)
bar: foo? B { $symbolstartpos }
foo: C nothing {}
(* Case: epsilon rule. *)
nothing: { $symbolstartpos }
