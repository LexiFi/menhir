(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/abstract.ml,v 1.2.8.1 1998/08/31 17:21:17 fpottier Exp $ *)
(*

Support for abstract type declarations. This module remembers each abstract type's arity, as well as the variance
of its parameters.

*)

open Errors

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A global table holds the known abstract type definitions and associates a unique stamp to each abstract type name.

*)

module Stamp = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

let stamps = ref (Stamp.empty : int Stamp.t);;

let new_stamp = 
  let current_stamp = ref 0 in
  function () ->
    let result = !current_stamp in
    current_stamp := succ result;
    result
;;

exception Unknown of string

let get_stamp name =
  try
    Stamp.find name !stamps
  with Not_found ->
    raise (Unknown name)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Since stamps start at zero and grow sequentially, we use an array to map a type's stamp to its name and to its
variance list. The array has to be re-allocated at each abstract type declaration, but that should be a rare
occurrence.

*)

let variances = ref [| |];;
let names = ref [| |];;

let new_variance variance =
  variances := Array.append !variances [| variance |]
;;

let new_name name =
  names := Array.append !names [| name |]
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Adding a new type to the table simply involves making sure that no type of that name exists. The function
returns the stamp of the newly created abstract type.

*)

exception Duplicate of string

let create name variance =
  try
    let _ = Stamp.find name !stamps in
    raise (Duplicate name)
  with Not_found ->
    let stamp = new_stamp() in
    stamps := Stamp.add name stamp !stamps;
    new_variance variance;
    new_name name;
    stamp
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Getting a type's variance list or its name, using its stamp.

*)

let get_variance stamp =
  (!variances).(stamp)
;;

let get_name stamp =
  (!names).(stamp)
;;
