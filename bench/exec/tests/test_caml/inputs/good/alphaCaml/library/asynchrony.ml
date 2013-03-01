(* This module provides the exception [Asynchrony]. *)

exception Asynchrony

let asynchrony =
  Asynchrony

let wrap f x1 x2 =
  try
    f () x1 x2;
    true
  with Asynchrony ->
    false

