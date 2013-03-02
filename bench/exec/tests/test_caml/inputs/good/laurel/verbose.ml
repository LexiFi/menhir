(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/verbose.ml,v 1.1 2000/02/25 15:01:34 fpottier Exp $ *)

(* Support for the [-v] option. *)
(* TEMPORARY take the flag into account *)

let say message =
  print_string message;
  flush stdout

