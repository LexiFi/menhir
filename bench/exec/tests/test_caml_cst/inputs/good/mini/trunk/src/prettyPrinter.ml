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

(* $Id: prettyPrinter.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module implements some common stuff for pretty printer. *)
open Format 

type output = 
  | Channel of Pervasives.out_channel
  | Buffer of Buffer.t 

type formatter_output = 
	{
	  out: string -> int -> int -> unit;
	  flush: unit -> unit;
	  newline: unit -> unit;
	  spaces: int -> unit;
	  with_tags: bool;
	  open_tag : tag -> unit;
	  close_tag : tag -> unit;
	  margin: int;
	}

type mode = 
      Latex of output
    | Txt of output
    | Formatter of formatter_output

let output_string output = 
  match output with
    | Channel cout -> Pervasives.output_string cout
    | Buffer b -> Buffer.add_string b

let flush output = 
  match output with
    | Channel cout -> Pervasives.flush cout
    | Buffer b -> ()

