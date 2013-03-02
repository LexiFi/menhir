(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/label.ml,v 1.5 2000/02/11 16:15:49 fpottier Exp $ *)

(* A type parser expects row labels to be strings. However, internally, it is possible to give them some more
   efficient representation, such as integers. Some module must allow conversions between the two formats. Such
   a module's signature is defined here. *)

module type S = sig

  (* The internal type of labels. *)

  type t

  (* [get] expects a string and returns the corresponding label, in internal form. *)

  val get: string -> t

  (* [print] expects a label and returns the corresponding string (i.e. the string which was passed to [get] when
     the label was created). *)

  val print: t -> string

end

(* Let us now give a trivial implementation of the above signature, where both operations are the identity. It has the
   advantage of simplicity, but may slow down operations, since it forces the type inference engine to perform many
   string comparisons while dealing with rows. *)

module Simple = struct

  type t = string

  let get s = s
  let print s = s

end

(* Lastly, here is a more interesting implementation of signature [S], where labels are integers.

   This sub-module implements a mapping from strings to integers. It is perfect, i.e. no two strings have the same
   image through the mapping. Of course, since there are many more strings than integers, the mapping is not total:
   some strings will be rejected. This peculiarity may be acceptable, if the probability of a clash is small enough.

   The formula used to hash strings into integers is taken from Jacques Garrigue's implementation of polymorphic
   variants, as described in the proceedings of the 1998 ML Workshop in Baltimore. A slight difference is that
   Garrigue's implementation signals a conflict only if two strings which lead to the same integer tag are used
   \emph{within a single row}, while ours works globally: all strings must have distinct hash codes within the
   whole session. The drawback of Garrigue's approach is that the inference engine itself must be made aware of
   these details -- they cannot be kept in a separate module, as done here. *)

module Integer = struct

  type t = int

  (* A hash table is used to keep track of all bindings created so far. It maps each integer hash code to the string
     which was bound to this code. *)

  let table =
    Hashtbl.create 2347

  (* This is Garrigue's hash function for strings. It guarantees that all 4 character identifiers will be given
     different hash values. *)

  let hash s =
    let rec sum power accu i =
      if i >= 0 then
	sum (223 * power) (accu + (Char.code s.[i]) * power) (i-1)
      else
	accu in
    sum 1 0 (String.length s - 1)

  (* [get s] returns an integer which uniquely identifies the string [s]. It raises [Clash] if [s] would receive
     the same hash code as another string previously passed to it. *)

  exception Clash of string * string

  let get s =
    let label = hash s in
    try
      let s' = Hashtbl.find table label in
      if s = s' then
	label
      else
	raise (Clash (s', s))
    with Not_found ->
      Hashtbl.add table label s;
      label

  (* [print label] returns the string associated to the hash code [label]. [label] must have been produced by a
     previous call to [get]; otherwise, [Not_found] is raised. *)

  let print =
    Hashtbl.find table

end

