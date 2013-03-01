(* $Header: /home/yquem/cristal/fpottier/cvs/picsou/currency.ml,v 1.1 2001/01/07 12:33:37 francois Exp $ *)

(* Currencies. *)

type currency =
  | Francs
  | Euros

(* Conversions. *)

let rate = 6.55957

let round price =
  (floor (price *. 100.)) /. 100.

let francs_to_euros price =
  round (price /. rate)

let euros_to_francs price =
  round (price *. rate)

