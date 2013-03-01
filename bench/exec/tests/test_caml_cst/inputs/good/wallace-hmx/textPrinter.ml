(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/textPrinter.ml,v 1.7 2000/02/11 16:15:53 fpottier Exp $ *)

module Make
    (Core: Core.S)

= struct

  open Format
  open Core

  let signs =
    ref true

  let rec int_to_alpha i =
    if i < 26 then String.make 1 (Char.chr (i+96))
    else (int_to_alpha (i/26) ^ String.make 1 (Char.chr ((i mod 26)+97)))

  type sign = Core.sign

  let variable sign index =
    let prefix = if !signs then
      match sign with
      | Bipolar -> "%"
      | Positive -> "+"
      | Negative -> "-"
      | Neutral -> "'"
    else "'" in
    print_string (prefix ^ (int_to_alpha index))

  let box p =
    printf "@[";
    p();
    printf "@]"

  let parentheses p =
    printf "@[(";
    p();
    printf "@])"

  let angle p =
    printf "@["; printf "<";
    p();
    printf "@]>"

  let comma () =
    printf ",@ "

  let colon () =
    printf ":@ "

  let semi () =
    printf ";@ "

  let less () =
    printf "@ <@ "

  let conditional hypothesis conclusion =
    printf "if ";
    hypothesis();
    printf "@ then@ ";
    conclusion()

  let newline =
    print_newline

  let label =
    print_string

  let urow () =
    printf "\\"

end

