(* $Id: position.ml 680 2007-05-25 13:41:41Z regisgia $ *)

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

open Lexing

type t = 
    { 
      start_p : Lexing.position; 
      end_p   : Lexing.position 
    }

type position = t

type 'a located =
    {
      value    : 'a;
      position : t;
    }

let value { value = v } =
  v

let position { position = p } =
  p

let destruct p =
  (p.value, p.position)

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let with_poss p1 p2 v =
  with_pos { start_p = p1; end_p = p2 } v

let map f v =
  {
    value     = f v.value;
    position  = v.position;
  }

let iter f { value = v } =
  f v

let mapd f v =
  let w1, w2 = f v.value in
  let pos = v.position in
    ({ value = w1; position = pos }, { value = w2; position = pos })
    
let dummy = 
  {
    start_p = Lexing.dummy_pos;
    end_p   = Lexing.dummy_pos
  }

let unknown_pos v = 
  {
    value     = v;
    position  = dummy
  }

let start_of_position p = p.start_p

let end_of_position p = p.end_p

let filename_of_position p = 
  p.start_p.Lexing.pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let join x1 x2 =
  {
    start_p = if x1 = dummy then x2.start_p else x1.start_p;
    end_p   = if x2 = dummy then x1.end_p else x2.end_p
  }

let lex_join x1 x2 =
  {
    start_p = x1;
    end_p   = x2
  }

let join_located l1 l2 f = 
  {
    value    = f l1.value l2.value;
    position = join l1.position l2.position;
  }

let string_of_lex_pos p = 
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let string_of_pos p = 
  let filename = filename_of_position p in
  let l = line p.start_p in
  let c1, c2 = characters p.start_p p.end_p in
    Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let pos_or_undef = function
  | None -> dummy
  | Some x -> x

let cpos lexbuf =
  { 
    start_p = Lexing.lexeme_start_p lexbuf;
    end_p   = Lexing.lexeme_end_p   lexbuf; 
  }

let with_cpos lexbuf v =
  with_pos (cpos lexbuf) v

let string_of_cpos lexbuf = 
  string_of_pos (cpos lexbuf)

let joinf f t1 t2 = 
  join (f t1) (f t2)

let ljoinf f =
  List.fold_left (fun p t -> join p (f t)) dummy

let join_located_list ls f = 
  {
    value     = f (List.map (fun l -> l.value) ls);
    position  = ljoinf (fun x -> x.position) ls
  }

