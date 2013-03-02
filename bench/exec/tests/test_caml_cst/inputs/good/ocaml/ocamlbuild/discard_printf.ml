(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: discard_printf.ml,v 1.1 2007-02-07 08:59:13 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
let rec greedy _ = greedy

let discard_printf _fmt = Obj.magic greedy
