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

(* $Id: infiniteArray.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module implements infinite arrays, that is, arrays that grow
    transparently upon demand. *)

type 'a t = {
    default: 'a;
    mutable table: 'a array
  } 

let default_size =
  16 (* must be non-zero *)

let make x = {
  default = x;
  table = Array.make default_size x
} 

let rec new_length length i =
  if i < length then
    length
  else
    new_length (2 * length) i

let ensure a i =
  let table = a.table in
  let length = Array.length table in
  if i >= length then begin
    let table' = Array.make (new_length (2 * length) i) a.default in
    Array.blit table 0 table' 0 length;
    a.table <- table'
  end

let get a i =
  ensure a i;
  a.table.(i)

let set a i x =
  ensure a i;
  a.table.(i) <- x

let iteri f a =
  Array.iteri f a.table
