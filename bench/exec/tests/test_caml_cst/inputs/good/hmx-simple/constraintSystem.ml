(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/constraintSystem.ml,v 1.11.4.1 2003/01/27 12:42:47 fpottier Exp $ *)

(* This signature describes the types and operations provided by the functor implemented in module [Herbrand]. *)

module type S = sig

  (* --------------------------------------------------------------------------------------------------------------- *)
  (* Type definitions. *)

  (* Terms of the free type algebra. *)

  type 'a absterm

  (* Row labels. *)

  type label

  (* Type nodes and type schemes. *)

  type node

  type term =
      node absterm

  type scheme

  (* --------------------------------------------------------------------------------------------------------------- *)
  (* Building types: variables, terms of the free algebra, row terms. *)

  (* [fresh()] returns a fresh node. *)

  val fresh: unit -> node

  (* [term term] returns a fresh node standing for the term [term]. *)

  val term: term -> node

  (* [juxtapose l v1 v2] returns a fresh node which stands for the row $(l: v_1; v_2)$. [replicate v]
     returns a fresh node which stands for the row $\urow v$. *)

  val juxtapose: label -> node -> node -> node
  val replicate: node -> node

  (* --------------------------------------------------------------------------------------------------------------- *)
  (* Unification. *)

  (* This exception is raised by the operation below if it causes the (implicit) global constraint set to become
     inconsistent. *)

  exception Inconsistency of string

  (* [unify node1 node2] creates an equality constraint between [node1] and [node2]. *)

  val unify: node -> node -> unit

  (* --------------------------------------------------------------------------------------------------------------- *)
  (* Generalization and instantiation. *)

  (* [scope action] executes the specified [action], with the side effect that all nodes freshly created during
     its scope are marked as such. *)

  val scope: (unit -> 'a) -> 'a

  (* [generalize v] creates a type scheme out of the constraints created during the current invocation of [exists],
     whose entry point is assumed to be [v]. *)

  val generalize: node -> scheme

  (* [instantiate scheme] creates a fresh instance of the type scheme [scheme]. It returns its entry point, and
     implicitly affects the global constraint set. *)

  val instantiate: scheme -> node

  (* [inject] turns a type node into a (trivial) type scheme. *)

  val inject: node -> scheme

  (* [capture] records the current generalization level and returns a function which, given a type node, lowers
     its level to the recorded level. *)

  val capture: unit -> (node -> unit)

  (* --------------------------------------------------------------------------------------------------------------- *)
  (* Printing type terms. *)

  module Print : sig

    (* [reset] resets the mechanism which assigns new names to type variables. *)

    val reset: unit -> unit

    (* [node] prints a type node, together with the constraints bearing on it. [scheme] prints a type scheme. *)

    val node: node -> string
    val scheme: scheme -> string

  end

end

