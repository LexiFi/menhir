(* $Id: uniqueIdentifier.ml 19 2007-09-25 14:23:43Z yann.regisgianas $ *)

(* Pangolin, a functional programming language for correct programs.
   Copyright (C) 2007, Yann Régis-Gianas, François Pottier.
   Contact: yann.regisgianas@gmail.com

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open PIdentifier

type t = PIdentifier.t

let ids = ref PIdentifier.Set.empty

let unique id =
  if PIdentifier.Set.mem id !ids then
    Error.error "during name analysis" (PIdentifier.position id)
      (Printf.sprintf "The name `%s' is already used and must be unique.\n"
	 (PIdentifier.as_string id))
  else 
    ids := PIdentifier.Set.add id !ids;
  id

let mk (pos, v) = 
  unique (PIdentifier.mk_predicate_id (pos, v))

let fresh (pos, v) = 
  unique (PIdentifier.fresh_predicate_id (pos, v))
  
let as_id x = x  
