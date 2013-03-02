(* $Id: error.ml 761 2007-07-02 22:17:39Z regisgia $ *)

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

let global_error kind msg = 
  Printf.eprintf "Global Error (%s)\n%s\n"
    kind
    msg;
  exit 1

let error kind pos msg = 
  Printf.eprintf "Error %s: (%s)\n%s\n"
    (Position.string_of_pos pos) 
    kind
    msg;
  exit 1
    
let error2 kind pos1 pos2 msg = 
  Printf.eprintf "Error %s:\n%s\n (%s)\n%s\n"
    (Position.string_of_pos pos1) 
    (Position.string_of_pos pos2) 
    kind
    msg;
  exit 1
