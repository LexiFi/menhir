(* $Header: /net/pauillac/caml/repository/bigbro/io_utils.ml,v 1.1.1.1 2001/02/13 15:39:37 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Read a whole file at once.

*)

let read_whole_file filename =
  let channel = open_in_bin filename in
  try
    let length = in_channel_length channel in
    let data = String.create length in
    really_input channel data 0 length;
    close_in channel;
    data
  with error ->
    close_in channel;
    raise error
;;
