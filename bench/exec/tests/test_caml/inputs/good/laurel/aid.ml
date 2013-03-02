(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/aid.ml,v 1.1 2000/03/01 07:44:27 fpottier Exp $ *)

(* This package helps deal with AID strings. *)

let hex4 x =
  let c = if x <= 9 then
    Char.chr (Char.code '0' + x)
  else
    Char.chr (Char.code 'a' - 0xa + x) in
  String.make 1 c

let hex8 x =
  "0x" ^ (hex4 (x lsr 4)) ^ (hex4 (x land 0xF))

let print aid =
  let buffer = Buffer.create 128 in
  Buffer.add_string buffer (hex8 (Char.code aid.[0]));
  for i = 1 to String.length aid - 1 do
    Buffer.add_char buffer ':';
    Buffer.add_string buffer (hex8 (Char.code aid.[i]))
  done;
  Buffer.contents buffer

