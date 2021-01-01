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

(* The library feat, which is found in the subdirectory feat/, has been
   renamed vendored_feat so as to prevent Dune from complaining about a
   conflict with a copy of feat that might be installed on the user's
   system. *)

(* As a result, the library is now accessible under the name Vendored_feat.
   Because we do not want to pollute our sources with this name, we define
   Feat as an alias for Vendored_feat. *)

include Vendored_feat
