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

open Grammar

type sentence =
  Nonterminal.t option * Terminal.t list

type located_sentence =
  Positions.positions * sentence

type comment =
  string

type 'a or_comment =
| Thing of 'a
| Comment of comment

let or_comment_iter f = function
  | Thing s ->
      f s
  | Comment _ ->
      ()

let or_comment_fold f accu = function
  | Thing s ->
      f accu s
  | Comment _ ->
      accu

let or_comment_map f = function
  | Thing s ->
      Thing (f s)
  | Comment c ->
      Comment c

let or_comment_filter_map f = function
  | Thing s ->
      Some (f s)
  | Comment _ ->
      None
