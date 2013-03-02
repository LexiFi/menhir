(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/tree.ml,v 1.2 2001/10/19 08:46:23 fpottier Exp $ *)

(* This module allows printing trees, while dealing with ambiguities, priorities and associativity in an abstract
   way. This addresses the issue of where to insert parentheses. *)

(* The client's implementation of [describe] (see below) must use this language to define the appearance of every
   constructor. *)

type 'a element =
  | Token of string
  | Son of 'a

(* The client must provide the following types and functions. *)

module type Input = sig

  (* The client must specify the algebra of input trees. *)

  type tree

  (* In order to implement priority and associativity rules, the client may wish to attach labels to the sons of a
     tree node. *)

  type label

  (* The physical appearance of every tree constructor is specified by [describe]. A call to [describe tree] must
     return a list of elements, in appropriate order, some of which (and at least one of which) are appropriate tokens
     (i.e. non-empty strings), and others are the direct sub-trees of the given tree, accompanied with user-defined
     labels. *)

  val describe: tree -> (label * tree) element list

  (* Priority and associativity is given by [parenthesize]. Given a label and a tree, this function tells how
     the tree should be parenthesized when it appears at this label. It does so by accepting a string (which
     is the printed representation of the tree) and parenthesizing it as desired. *)

  val parenthesize: label -> tree -> string -> string

end

module Make (X : Input) = struct

  (* Turning an (input) tree into a string. *)

  let rec print node =
    List.fold_left (fun result element -> result ^ (
      match element with
      |	Token token ->
	  token
      |	Son (label, son) ->
	  X.parenthesize label son (print son)
    )) "" (X.describe node)

end

(* Sample parenthesizing functions. *)

let nothing s =
  s

let parentheses s =
  "(" ^ s ^ ")"

let angle_brackets s =
  "<" ^ s ^ ">"

