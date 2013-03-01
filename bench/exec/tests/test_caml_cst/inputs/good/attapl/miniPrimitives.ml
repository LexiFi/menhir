(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/miniPrimitives.ml,v 1.2 2004/04/09 16:11:44 fpottier Exp $ *)

type t =
  | PIntegerConstant of int
  | PAdd

type primitive = t

type variable = MiniSolver.variable

type crterm = MiniSolver.crterm

open MiniSolver
open MiniAlgebra

let generate t args = function
  | PIntegerConstant _ ->
      (int =?= t) ^
      args []
  | PAdd ->
      (int =?= t) ^
      args [ int; int ]

