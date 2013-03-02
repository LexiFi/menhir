(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/error.ml,v 1.1 2002/05/30 15:29:18 fpottier Exp $ *)

(* Some error handling facilities. *)

let abort action =
  action();
  flush_all();
  exit(1)

let (>>) action1 action2 () =
  action1();
  action2()

