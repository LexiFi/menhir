(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/signature.ml,v 1.7 2000/02/11 16:15:36 fpottier Exp $ *)

(*i*)

open Syntax

(*i*)

(* This module provides internal type definitions for Gromit's type algebra specifications. *)

(* This sub-module offers sets of strings. *)

module StringSet = Set.Make (struct
  type t = string
  let compare = compare
end)

(* A lattice consists of the following components:
   \begin{itemize}
   \item a name;
   \item a symbol count;
   \item an array which maps symbol numbers to symbol information structures;
   \item a hash table which maps label names to their variance and their to their kind;
   \item an [Ordering.lattice] structure which describes the lattice.
   \end{itemize} *)

type lattice = {
    lattice_name : string;
    lattice_count : int;
    lattice_symbol : symbol_info array;
    lattice_label : (string, variance * sort * string) Hashtbl.t;
    lattice_lattice : Ordering.lattice
  } 

(* Symbol information consists of:
   \begin{itemize}
   \item a symbol name;
   \item a symbol arity, expressed as a set of label names;
   \item a symbol appearance, expressed as a list of labels and tokens;
   \item a symbol priority, expressed as an integer. The lower the integer, the stronger the symbol.
   \end{itemize} *)

and symbol_info = {
    mutable symbol_name : string;
    mutable symbol_arity : StringSet.t;
    mutable symbol_appearance : appearance;
    mutable symbol_priority: int
  } 

(* A signature is a list of lattices. *)

type signature = lattice list

