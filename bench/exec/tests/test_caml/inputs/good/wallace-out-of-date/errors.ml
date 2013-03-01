(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/errors.ml,v 1.1.8.2 1999/04/05 16:45:38 francois Exp $ *)
(*

General-purpose error handling.

*)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A general-purpose exception to be used whenever an assertion is violated. Errors.trace helps printing interesting
debugging messages when an exception is raised. All calls to functions which can potentially raise exceptions,
directly or indirectly, should be wrapped with a call to Errors.trace.

The first parameter is a piece of text which identifies the caller, typically the calling function's full name.
The second parameter is the action to be performed while watching for errors, and the result is the value obtained
by executing it.

Munge_exception can be changed to your liking when tracking a specific bug, so you can let go of certain exceptions
and watch others. By default, it looks only for CantHappen, but can be used for other exceptions. Errors.trace is
small enough that it should be inlined by the compiler, resulting in a minimal time loss.

*)

exception CantHappen of string

let watch name = function
    CantHappen _ ->
      prerr_endline ("Exiting trace point: " ^ name)
  | _ ->
      ()

let trace name action =
  try
    action()
  with error ->
    watch name error;
    raise error

