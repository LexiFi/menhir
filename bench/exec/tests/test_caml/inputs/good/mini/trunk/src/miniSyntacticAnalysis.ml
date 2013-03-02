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

(* $Id: miniSyntacticAnalysis.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module provides a parser for program and a parser for constraint. *)

open Misc
open ParsingExceptions

let parse_program_task = "parse-program"

let parse_constraint_task = "parse-constraint"

let filename = ref None

let what_file fname =
  filename := Some fname

let has_file () = 
  !filename <> None

let parse_program_from_channel filename lexer = 
  (* Prepare the lexer. *)
    lexer.Lexing.lex_curr_p <- 
      {Lexing.pos_fname = filename; Lexing.pos_lnum = 1;
       Lexing.pos_bol = 0; Lexing.pos_cnum = 0};
    (* And parse the token stream. *)
    try
      MiniParser.program MiniLexer.token lexer
    with Parsing.Parse_error -> raise (Other lexer)

let parse_program_file () =
  let filename = unSome (!filename) in
    parse_program_from_channel filename 
      (Lexing.from_channel (open_in filename))

let parse_program_from_string s = 
  parse_program_from_channel "" (Lexing.from_string s)

let parse_constraint_from_channel filename lexer =
  (* Prepare the lexer. *)
    lexer.Lexing.lex_curr_p <- 
      {Lexing.pos_fname = filename; Lexing.pos_lnum = 1;
       Lexing.pos_bol = 0; Lexing.pos_cnum = 0};
    (* And parse the token stream. *)
    try
      let c, env = MiniInfer.init_env () in
	c (ConstraintParser.tconstraint ConstraintLexer.token lexer
	(env, MultiEquation.init ()))
    with Parsing.Parse_error -> raise (Other lexer)

let parse_constraint_file () =
  let filename = unSome (!filename) in
    parse_constraint_from_channel filename 
      (Lexing.from_channel (open_in filename))

let parse_constraint_from_string s = 
  parse_constraint_from_channel "" (Lexing.from_string s)

let register_tasks () = 
  Processing.register 
    parse_program_task ([], what_file) [] parse_program_file has_file;
  Processing.register 
    parse_constraint_task ([], what_file) [] parse_constraint_file has_file


