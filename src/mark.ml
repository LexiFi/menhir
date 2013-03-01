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

