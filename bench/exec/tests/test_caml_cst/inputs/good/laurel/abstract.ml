(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/abstract.ml,v 1.3 2000/02/29 08:39:41 fpottier Exp $ *)

(* This module defines a more abstract instruction set, where several families of instructions have been collapsed,
   but which is still suitable for our analysis.

   [If m] represents an operator which takes $m$ words of arguments from the stack and uses them to branch among
   (possibly) several target nodes. It \emph{never} throws an exception. The choice of the target node shall be
   assumed to be dependent on \emph{every} argument during the information flow analysis. $m$ may equal $0$.

   [StackOp(m, n)] represents an operator which takes $m$ words of arguments from the stack, produces $n$ words of
   results, and \emph{never} throws and exception. \emph{Every} result shall be assumed to be dependent on
   \emph{every} argument during the information flow analysis. *)

open JavaCard

(* The type of instructions. *)

type opcode =
  | Arraylength
  | Arrayload of absi
  | Arraystore of absi
  | Athrow
  | Checkcast of check
  | Dup_x of int * int
  | Dup2
  | Getfield of absi * this * cpx
  | Getstatic of absi * cpx
  | Idivrem
  | If of int
  | Inc of si * index
  | Instanceof of check
  | Invokeinterface of int * cpx * token
  | Invokespecial of cpx
  | Invokestatic of cpx
  | Invokevirtual of cpx
  | Jsr
  | Load of asi * index
  | New of cpx
  | Newarray of jarray
  | Putfield of absi * this * cpx
  | Putstatic of absi * cpx
  | Ret of index
  | Return of asi option
  | Sdivrem
  | StackOp of int * int
  | Store of asi * index
  | Swap_x of int * int

