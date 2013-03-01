(* $Header: /net/pauillac/caml/repository/bigbro/string_utils.ml,v 1.1.1.1 2001/02/13 15:39:34 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This module implements a subset of module String's interface. It is designed to allow working with substrings of
a fixed, immutable string.

*)

module Substring = struct

  type t = string * int * int

  let length (_, _, length) =
    length
  ;;

  let get (string, start, length) index =
    String.get string (start + index)
  ;;

  let valid_substring message (_, _, length) start' length' =
    if (start' < 0) or (length' < 0) or (start' + length' > length) then
      raise (Invalid_argument message)
  ;;

  let sub ((string, start, length) as substring) start' length' =
    valid_substring "Substring.sub" substring start' length';
    (string, start + start', length')
  ;;

  let blit ((string, start, length) as substring) offset1 string2 offset2 len =
    valid_substring "Substring.blit" substring offset1 len;
    String.blit string (start + offset1) string2 offset2 len
  ;;

  (* Conversion functions. *)
  
  let of_string string =
    (string, 0, String.length string)
  ;;

  let to_string (string, start, length) =
    String.sub string start length
  ;;

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Now, start writing a functor which can be applied to strings or substrings.

*)

module type CommonSig = sig
  type t

  val split: t -> int -> t * t
  val chop: (char -> bool) -> t -> t * char * t
  val filter: (char -> bool) -> t -> t * t
  val iter: (char -> unit) -> t -> unit
  val fold: ('a -> char -> 'a) -> 'a -> t -> 'a
  val flatten: t list -> string
end

module CommonFunctor (Param : sig
  type t
  
  val length: t -> int
  val get: t -> int -> char
  val sub: t -> int -> int -> t
  val blit: t -> int -> string -> int -> int -> unit
end) =
struct

  type t = Param.t

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

split splits the specified (sub)string at the specified offset. It can raise Invalid_argument.

*)

  let split text offset =
    Param.sub text 0 offset, Param.sub text offset (Param.length text - offset)
  ;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function looks for the first character which satisfies the given predicate. If none exists, it raises Not_found.
Otherwise, it returns the first part of the (sub)string, the character itself, and the second part of the (sub)string.

*)

  let chop predicate text =
    let length = Param.length text in
    let rec look index =
      if index < length then begin
	let c = Param.get text index in
	let succindex = index + 1 in
	if predicate c then
	  (Param.sub text 0 index, c, Param.sub text succindex (length - succindex))
	else
	  look succindex
      end
      else
	raise Not_found
    in look 0
  ;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

filter cuts the (sub)string in two parts: the characters which verify the predicate, and those which follow. It does
not raise any exception.

*)

  let filter predicate text =
    let length = Param.length text in
    let rec first_invalid index =
      if index = length then index
      else if predicate (Param.get text index) then first_invalid (succ index)
      else index in

    let index = first_invalid 0 in
    split text index
  ;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

iter iterates the supplied function on each character of the supplied (sub)string.
fold does the same, but maintains an accumulator.

*)

 let iter action string =
   for index = 0 to Param.length string - 1 do
     action (Param.get string index)
   done
 ;;

 let fold action accu string =
   let length = Param.length string in
   let rec loop accu index =
     if index = length then accu
     else loop (action accu (Param.get string index)) (succ index) in
  loop accu 0
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

flatten converts a list of (sub)strings to a string, in linear time.

*)

let flatten list =

  let rec size accu = function
      [] -> accu
    | elem :: rest -> size (accu + Param.length elem) rest in

  let string = String.create (size 0 list) in

  let rec copy index = function
      [] -> ()
    | elem :: rest ->
	let length = Param.length elem in
	Param.blit elem 0 string index length;
	copy (index + length) rest in

  copy 0 list;
  string
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

End of the functor. Apply it twice.

*)

end

module StringOp = CommonFunctor (struct
  type t = string
  let length = String.length;;
  let get = String.get;;
  let sub = String.sub;;
  let blit = String.blit;;
end)

module SubstringOp = CommonFunctor (Substring)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Shorten returns its parameter unchanged if it is shorter than the specified limit; otherwise, it removes its middle
portion and put an ellipsis in its place. The limit must be 3 at least.

*)

let shorten limit text =
  let length = String.length text in
  if length <= limit then text
  else begin
    if limit < 3 then
      raise (Invalid_argument "String_utils.shorten");
    let half1 = (limit - 3) / 2 in
    let half2 = limit - 3 - half1 in
    (String.sub text 0 half1) ^ "..." ^ (String.sub text (length - half2) half2)
  end
;;
