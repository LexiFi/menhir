(* $Id: options.ml 18 2007-09-25 14:03:21Z yann.regisgianas $ *)

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

type 'a description = {
  env         : 'a;
  postprocess : 'a -> 'a;
  options     : (Arg.key * Arg.spec * Arg.doc) list
}

module Locals = struct type 'a t = 'a description end

module ExLocals = Misc.ExMake (Locals)

type t = ExLocals.t

let local_options_descr x = 
  ExLocals.use { ExLocals.f = fun x -> x.options } x

let mk_local_options d = 
  ExLocals.pack d

let process usage (options : ExLocals.t list) = 
  let arguments = ref [] in
  let options = List.flatten (List.map local_options_descr options) in
    Arg.parse options (Misc.augment arguments) usage;
    !arguments

