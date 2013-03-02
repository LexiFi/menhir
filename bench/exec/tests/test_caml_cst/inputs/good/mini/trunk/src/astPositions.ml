(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: astPositions.ml 421 2006-12-22 09:27:42Z regisgia $ *)

open Positions
open MiniAst

let position = function
  | EAssertFalse p -> p
  | EForall (p, _, _) -> p
  | EExists (p, _, _) -> p
  | EVar (p, _) -> p
  | EDCon (p, _, _) -> p
  | ELambda (p, _, _) -> p
  | EApp (p, _, _) -> p
  | EMatch (p, _, _) -> p
  | EBinding (p, _, _) -> p
  | ETypeConstraint (p, _, _) -> p
  | ERecordEmpty p -> p
  | ERecordAccess (p, _, _) -> p
  | ERecordExtend (p, _, _) -> p
  | ERecordUpdate (p, _, _, _) -> p
  | EPrimApp (p, _, _) -> p

let joine = joinf position

let lposition = ljoinf position

let tposition = function
  | TypVar (p, _) -> p
  | TypApp (p, _, _) -> p
  | TypRowCons (p, _, _) -> p
  | TypRowUniform (p, _) -> p

let tjoin = joinf tposition

let tlposition = ljoinf tposition

let bposition = function
  | BindValue (p, _) -> p
  | BindRecValue (p, _) -> p
  | TypeDec (p, _) -> p

let bjoin = joinf bposition

let blposition = ljoinf bposition

let vposition = function
  | (p, _, _, _) -> p

let vlposition p = ljoinf vposition p

let tdposition = function
  | (p, _, _, _) -> p

let tdlposition p = ljoinf tdposition p

let pposition = function
  | PVar (p, _) -> p
  | PWildcard p -> p
  | PAlias (p, _, _) -> p
  | PTypeConstraint (p, _, _) -> p
  | PData (p, _, _) -> p
  | POr (p, _) -> p
  | PAnd (p, _) -> p
  | PPrimitive (p, _) -> p

let pjoin = joinf pposition

let plposition = ljoinf pposition

