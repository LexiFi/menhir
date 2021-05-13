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

open Dot
open StackLang
open StackLangUtils

(* [uniq] transforms an arbitrary [iter] function into one that produces each
   element at most once. *)

let uniq iter =
  let encountered = ref LabelSet.empty in
  fun yield ->
    iter (fun label ->
        if not (StringSet.mem label !encountered)
        then begin
          encountered := StringSet.add label !encountered;
          yield label
        end )


let print program =
  let module P =
    Dot.Print (struct
      type vertex = label

      let name label = label

      let successors (f : ?style:style -> label:string -> vertex -> unit) label
          =
        (lookup label program.cfg).block
        |> uniq Block.successors (fun target -> f ~label:"" target)


      let iter
          (f : ?shape:shape -> ?style:style -> label:string -> vertex -> unit) =
        program.cfg
        |> LabelMap.iter (fun label _block -> f ~shape:Box ~label label)
    end)
  in
  let f = open_out (Settings.base ^ ".dot") in
  P.print f;
  close_out f
