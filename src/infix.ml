(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

let ( += ) r i = r := !r + i

let ( *= ) r i = r := !r * i

let ( -= ) r i = r := !r - i

let ( /= ) r i = r := !r / i

let ( ||= ) r i = r := !r || i

let ( &&= ) r i = r := !r && i

let ( @:= ) r f = r := f !r

let ( @@> ) f g x = f (g x)
