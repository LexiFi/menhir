(* $Header: /net/pauillac/caml/repository/bigbro/base64.ml,v 1.2 2001/03/01 16:00:33 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This exception is raised by the decoder iff the input stream is ill-formed.

*)

exception Malformed

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Decoding.

The decoding array does not need to be protected by a lock, because it is read-only. It maps the ASCII code of a
base64 encoded value to the value itself, which is a 6-bit quantity. Entries which do not correspond to valid base64
codes contain 64, which is an invalid 6-bit quantity. This is used to detect invalid inputs.

*)

let index64 = Array.create 256 64
let _ =
  for i = 0 to 25 do index64.(i + Char.code 'A') <- i done;
  for i = 0 to 25 do index64.(i + Char.code 'a') <- i + 26 done;
  for i = 0 to 9 do  index64.(i + Char.code '0') <- i + 52 done;
  index64.(Char.code '+') <- 62;
  index64.(Char.code '/') <- 63
;;

let decode1 char =
  let value = index64.(Char.code char) in
  if value = 64 then
    raise Malformed;
  value
;;

let decode string =

  (* Base64 strings are divided in groups of 4 characters (24 actual bits of data). *)

  let src_length = String.length string in
  if (src_length mod 4 <> 0) then
    raise Malformed;

  (* Each group corresponds to 3 data bytes. *)

  let dst_length = src_length / 4 * 3 in
  let data = String.create dst_length in

  (* The decoding loop. *)

  let rec loop src_index dst_index =
    if src_index < src_length then begin

      (* Read a 4-character group. *)
      
      let v1 = decode1 string.[src_index] in
      let v2 = decode1 string.[src_index + 1] in
      let v3 = decode1 string.[src_index + 2] in
      let v4 = decode1 string.[src_index + 3] in

      (* Turn it into a 24-bit quantity. *)

      let quantum = (v1 lsl 18) lor (v2 lsl 12) lor (v3 lsl 6) lor v4 in

      (* Decompose it into 3 bytes. *)

      data.[dst_index] <- Char.chr (quantum lsr 16);
      data.[dst_index+1] <- Char.chr ((quantum lsr 8) land 0xFF);
      data.[dst_index+2] <- Char.chr (quantum land 0xFF);

      (* Continue. *)

      loop (src_index + 4) (dst_index + 3)
    end in

  loop 0 0;

  (* Deal with the end. If the number of bytes that was encoded is not a multiple of 3, then the base64 stream
     ends with 1 or 2 '=' signs. The calls to [String.sub] can't fail. *)

  if string.[src_length - 1] = '=' then
    if string.[src_length - 2] = '=' then
      String.sub data 0 (dst_length - 2)
    else
      String.sub data 0 (dst_length - 1)
  else
    data
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Encoding.

The encoding array maps any 6-bit quantity to its associated 8-bit base64 code.

*)

let char64 = Array.create 64 'a'
let _ =
  for i = 0 to 25 do char64.(i) <- Char.chr (Char.code 'A' + i) done;
  for i = 0 to 25 do char64.(i+26) <- Char.chr (Char.code 'a' + i) done;
  for i = 0 to 9 do char64.(i+52) <- Char.chr (Char.code '0' + i) done;
  char64.(62) <- '+';
  char64.(63) <- '/'
;;

let encode data =
  let true_src_length = String.length data in

  (* If the length of the input data isn't a multiple of 3, pretend it is by adding null bytes. We shall also
     tweak the output by putting an appropriate number of '=' signs at the end of the output string. *)

  let data, src_length = match true_src_length mod 3 with
    0 -> data, true_src_length
  | 1 -> data ^ "\000\000", true_src_length + 2
  | 2 -> data ^ "\000", true_src_length + 1
  | _ -> assert false in

  (* Compute the length of the encoded string, and allocate it. *)

  let dst_length = src_length / 3 * 4 in
  let string = String.create dst_length in

  (* The encoding loop. *)

  let rec loop src_index dst_index =
    if src_index < src_length then begin

      (* Read a 3 byte group. *)

      let b1 = Char.code data.[src_index] in
      let b2 = Char.code data.[src_index+1] in
      let b3 = Char.code data.[src_index+2] in

      (* Create a 24 bit quantity. *)

      let quantum = (b1 lsl 16) lor (b2 lsl 8) lor b3 in

      (* Decompose it into 4 6-bit quantities, and use the array to encode them. *)

      string.[dst_index] <- char64.((quantum lsr 18) land 0x3f);
      string.[dst_index+1] <- char64.((quantum lsr 12) land 0x3f);
      string.[dst_index+2] <- char64.((quantum lsr 6) land 0x3f);
      string.[dst_index+3] <- char64.(quantum land 0x3f);

      (* Continue. *)

      loop (src_index + 3) (dst_index + 4)
    end in

  loop 0 0;

  (* It remains to modify the output by adding an appropriate number of '=' signs. *)

  for i = 1 to src_length - true_src_length do
    string.[dst_length - i] <- '='
  done;

  string
;;
