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

(* $Id: positions.ml 421 2006-12-22 09:27:42Z regisgia $ *)

open Lexing

type position = 
    { 
      start_p : Lexing.position; 
      end_p   : Lexing.position 
    }

let undefined_position = 
  {
    start_p = Lexing.dummy_pos;
    end_p   = Lexing.dummy_pos
  }

let start_of_position p = p.start_p

let end_of_position p = p.end_p

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  if line p1 = line p2 then
    (column p1, column p2)
  else 
    (column p1, column p1 + 1)

let join x1 x2 =
{
  start_p = if x1 = undefined_position then x2.start_p else x1.start_p;
  end_p   = if x2 = undefined_position then x1.end_p else x2.end_p
}

let lex_join x1 x2 =
{
  start_p = x1;
  end_p   = x2
}

let string_of_characters (c1, c2) =  
  (string_of_int c1)^"-"^(string_of_int c2)
    
let string_of_lex_pos p = 
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let string_of_pos p = 
  (if p.start_p.pos_fname <> "" then "File \""^p.start_p.pos_fname^"\", "
   else "")
  ^"line "^(string_of_int p.start_p.pos_lnum)
  ^", characters "^ string_of_characters (characters p.start_p p.end_p)

let pos_or_undef = function
  | None -> undefined_position
  | Some x -> x

let cpos lexbuf =
  { 
    start_p = Lexing.lexeme_start_p lexbuf;
    end_p   = Lexing.lexeme_end_p   lexbuf; 
  }

let string_of_cpos lexbuf = 
  string_of_pos (cpos lexbuf)

let joinf f t1 t2 = 
  join (f t1) (f t2)

let ljoinf f =
  List.fold_left (fun p t -> join p (f t)) undefined_position

