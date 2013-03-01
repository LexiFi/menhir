(* $Id: pIdentifier.ml 43 2007-10-01 14:35:01Z yann.regisgianas $ *)

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

type salt = int

let no_salt : salt = -1

type kind = 
  | ValueConstructor 
  | Value 
  | Type 
  | Predicate 
  | Formula
  | LogicFunction

type t = kind * string * salt * Position.t

type identifier = t

let string_of_kind = function
  | Value -> "value"
  | ValueConstructor -> "data constructor"
  | Type -> "type"
  | Predicate -> "predicate"
  | Formula -> "formula"
  | LogicFunction -> "logic function"

let as_string (k, s, i, pos) =
  s ^ if i <> no_salt && i <> 0 then string_of_int i else "" 

let as_string' (k, s, i, pos) =
  Printf.sprintf "%s_%d (%s - %s)"
    s i (string_of_kind k) (Position.string_of_pos pos)

let compare ((k, s, i, _) as _s1) ((k', s', i', _) as _s2) = 
  compare (k, s, i) (k', s', i') 

let basename (k, s, _, pos) = 
  (k, s, no_salt, pos)

let combine (k, s, _, pos) x = 
  (k, s, x, pos)

let position (_, _, _, pos) = 
  pos

exception AlreadyPresent
  
module Map = 
  struct 
    include Map.Make (struct type t = identifier 
			     let compare = compare
		      end)
    let union m1 m2 = 
      fold add m1 m2

    let unique_add x s = 
      if not (mem x s) then
	add x s
      else 
	raise AlreadyPresent

  end

module Set = 
  Set.Make (struct type t = identifier 
		   let compare = compare
	    end)

let set_unique_add x s = 
  if Set.mem x s then raise AlreadyPresent else Set.add x s

let distinct l = 
  try 
    ignore (List.fold_left (fun s x -> set_unique_add x s) Set.empty l); 
    true
  with AlreadyPresent -> false

let mk_value_id ?(is_constructor=false) (pos, x) = 
  if is_constructor then 
    (ValueConstructor, x, no_salt, pos)
  else 
    (Value, x, no_salt, pos)

let is_constructor_id (k, _, _, _) =
  k = ValueConstructor

let mk_formula_id (pos, x) = 
  (Formula, x, no_salt, pos)

let mk_type_id (pos, x) = 
  (Type, x, no_salt, pos)

let mk_predicate_id (pos, x) = 
  (Predicate, x, no_salt, pos)

let mk_logic_function_id (pos, x) = 
  (LogicFunction, x, no_salt, pos)

let salts = Hashtbl.create 14

let reset () = Hashtbl.clear salts

let fresh_salt i =
  let g = (try Hashtbl.find salts i with Not_found -> -1) + 1 in
    Hashtbl.replace salts i g;
    g

let fresh_value_id (pos, i) = 
  (Value, i, fresh_salt i, pos)

let fresh_value_id' (pos, i) = 
  let s = fresh_salt i in
  (Value, i ^ string_of_int s, s, pos)

let fresh_type_id (pos, i) = 
  (Type, i, fresh_salt i, pos)

let fresh_predicate_id (pos, i) = 
  (Predicate, i, fresh_salt i, pos)

let fresh (k, i, _, pos) = 
  (k, i, fresh_salt i, pos)

let freshb (k, _, _, _) s = 
  (k, s, fresh_salt s, Position.dummy)
  
let equal i1 i2 = 
  compare i1 i2 = 0

let is_type (k, _, _, _) = 
  k = Type
