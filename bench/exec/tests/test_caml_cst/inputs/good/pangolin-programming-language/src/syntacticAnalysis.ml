(* $Id: syntacticAnalysis.ml 45 2007-10-01 14:36:53Z yann.regisgianas $ *)

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

let process filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
    lexbuf.Lexing.lex_curr_p <-
      {
        Lexing.pos_fname = filename;
        Lexing.pos_lnum  = 1;
        Lexing.pos_bol   = 0;
        Lexing.pos_cnum  = 0
      };
    Parser.program Lexer.main lexbuf

let process filename = try 
  process filename
with _ -> 
  Error.global_error "file loading" 
    (Printf.sprintf "`%s' does not exist." filename)
