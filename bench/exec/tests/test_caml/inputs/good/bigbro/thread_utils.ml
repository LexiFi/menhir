(* $Header: /net/pauillac/caml/repository/bigbro/thread_utils.ml,v 1.1.1.1 2001/02/13 15:39:35 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A utility which protects an operation with a mutex.

*)

let protect lock action argument =
  Mutex.lock lock;
  try
    let result = action argument in
    Mutex.unlock lock;
    result
  with error ->
    Mutex.unlock lock;
    raise error
;;
