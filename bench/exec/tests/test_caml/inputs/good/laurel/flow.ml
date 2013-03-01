(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/flow.ml,v 1.5 2000/01/11 16:34:34 fpottier Exp $ *)

(* This module defines the type of the labels found on flow edges during the bytecode analysis.

   [Normal] edges correspond to regular flow edges, including subroutine call and return edges. [Throw] edges
   correspond to jumps to an exception handler, due to some exception being thrown. [Pseudo] edges link every
   \texttt{jsr} instruction to the instruction immediately following it. [Imaginary] edges are used to connect
   [return] nodes to imaginary nodes, and imaginary nodes between them. They are parameterized by the number of
   stack words which they preserve. *)

type label =
  | Normal
  | Throw
  | Pseudo
  | Imaginary of int

