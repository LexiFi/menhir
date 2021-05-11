(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open Grammar
open StackLang

(* This module offers an interpreter for the intermediate language StackLang. *)

(* The parameters are a StackLang program, a start label, a Boolean flag that
   tells whether a trace should be produced on the standard error channel, a
   lexer, and a lexing buffer. The interpreter either succeeds or fails. It
   returns nothing beyond this single bit of information. *)

val interpret :
     (* program: *) program
  -> (* entry:   *) label
  -> (* trace:   *) bool
  -> (* lexer:   *) (lexbuf -> Terminal.t)
  -> (* lexbuf:  *) lexbuf
  -> unit option
