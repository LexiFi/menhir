(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/mark.ml,v 1.2 2003/04/02 16:52:27 fpottier Exp $ *)

(** This module implements a very simple notion of ``mark''. A mark is
    really a reference cell (without content). Creating a new mark
    requires allocating a new cell, and comparing marks requires
    comparing pointers. *)

type t =
    unit ref

let fresh =
  ref

let same =
  (==)

let none =
  fresh()

