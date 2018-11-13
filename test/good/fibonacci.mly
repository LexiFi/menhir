%token A
%start<unit> fib

%%

(* An intensive inlining test.
   The size of the grammar after inlining is exponential.
   For efficiency, inlining must be performed bottom-up,
   beginning by inlining [fib0] and [fib1] into [fib2],
   then inlining [fib2] at its use site, and so on.
   A top-down strategy, without memoization, would cause
   repeated work. *)

let fib0 == A
let fib1 == A
let fib2 == fib0; fib1
let fib3 == fib1; fib2
let fib4 == fib2; fib3
let fib5 == fib3; fib4
let fib6 == fib4; fib5
let fib7 == fib5; fib6
let fib8 == fib6; fib7
let fib9 == fib7; fib8

let fib  := fib9
