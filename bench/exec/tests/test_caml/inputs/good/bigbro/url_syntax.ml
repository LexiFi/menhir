(* $Header: /net/pauillac/caml/repository/bigbro/url_syntax.ml,v 1.2 2001/03/01 16:00:34 fpottier Exp $ *)

open String_utils

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This is the only exception raised by this module.

*)

exception Malformed of string

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Converting hex digits to a number, and vice versa.

*)

let hex_to_quad c =
  if (c >= '0') & (c <= '9') then Char.code c - 0x30 (* Char.code '0' *)
  else if (c >= 'a') & (c <= 'f') then Char.code c - 0x61 (* Char.code 'a' *) + 0x0a
  else if (c >= 'A') & (c <= 'F') then Char.code c - 0x41 (* Char.code 'A' *) + 0x0A
  else
    raise (Malformed ("Invalid hex digit: '" ^ (Char.escaped c) ^ "'."))
;;

let quad_to_hex q =
  if (q >= 0) & (q <= 9) then Char.chr (q + 0x30 (* Char.code '0' *))
  else if (q >= 0x0a) & (q <= 0x0f) then Char.chr (q - 0x0a + 0x41 (* Char.code 'A' *))
  else assert false
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

For simplicity, we convert strings to list of characters. There are two kinds of characters, raw characters and
escape sequences.

*)

type character =
    Raw of char
  | Escaped of char

let characters_of_string escape_aware string =
  let length = String.length string in

  let rec build_aware index =
    if index = length then []
    else match string.[index] with
      '%' ->
	if index + 2 >= length then begin
	  let sequence = String.sub string index (length - index) in
	  raise (Malformed ("Malformed escape sequence \"" ^ (String.escaped sequence)
			    ^ "\": % sign should be followed by two hex digits."))
	end;
	let element = Escaped (Char.chr ((hex_to_quad string.[index+1]) * 0x10 + (hex_to_quad string.[index+2]))) in
	element :: (build_aware (index + 3))
    | c ->
	(Raw c) :: (build_aware (succ index))

  and build index =
    if index = length then []
    else (Raw string.[index]) :: (build (succ index)) in

  (if escape_aware then build_aware else build) 0
;;

let string_of_character = function
    Raw c ->
      String.make 1 c
  | Escaped c ->
      let code = Char.code c in
      let sequence = String.create 3 in
      sequence.[0] <- '%';
      sequence.[1] <- quad_to_hex (code / 0x10);
      sequence.[2] <- quad_to_hex (code mod 0x10);
      sequence
;;

let code_of_character = function
  | Raw c ->
      Char.code c
  | Escaped c ->
      Char.code c

let string_of_characters characters =
  StringOp.flatten (List.map string_of_character characters)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

These predicates correspond to the HTTP-1.1 standard (RFC 2068).

*)

let alpha = function
    Raw (('a'..'z') | ('A'..'Z')) -> true
  | _ -> false
;;

let digit = function
    Raw ('0'..'9') -> true
  | _ -> false
;;

let safe = function
    Raw ('$' | '-' | '_' | '.') -> true
  | _ -> false
;;

let extra = function
    Raw ('!' | '*' | '\'' | '(' | ')' | ',') -> true
  | _ -> false
;;

let reserved = function
    Raw (';' | '/' | '?' | ':' | '@' | '&' | '=' | '+') -> true
  | _ -> false
;;

let control = function
    Raw c when (c <= '\031') or (c >= '\127') -> true
  | _ -> false
;;

let unsafe = function
    Raw (' ' | '"' | '#' | '%' | '<' | '>') -> true
  | c -> control c
;;

let unreserved c =
  (not (reserved c)) & (not (unsafe c))
;;

let uchar = function
    Escaped _ -> true
  | c -> unreserved c
;;

let pchar = function
    Raw (':' | '@' | '&' | '=' | '+') -> true
  | c -> uchar c
;;

let scheme = function
    Raw ('+' | '-' | '.') -> true
  | c -> (alpha c) or (digit c)
;;

let net_loc = function
    Raw (';' | '?') -> true
  | c -> pchar c
;;

let query c =
  (uchar c) or (reserved c)
;;

let fragment c =
  (uchar c) or (reserved c)
;;

let param = function
    Raw ('/' | ';') -> true
  | c -> pchar c
;;

let segment c =
  pchar c
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function takes a string, converts to an internal representation, makes sure that each of the characters match
the specified predicate. Then, any unnecessary escape sequences are converted back to raw characters, so that future
comparisons need not take escape sequences into account. (An escape sequence is unnecessary if the character it
encodes is accepted by the predicate.)

*)

let normalize predicate location string =

  (* Before converting the string to a list of characters, we must determine whether escape sequences are to be
     recognized as such. To do this, we check whether the predicate accepts escape sequences. *)
  
  let escape_aware = predicate (Escaped 'a') in

  (* Convert the string to characters. *)

  let characters = characters_of_string escape_aware string in

  (* In one pass on the list of characters, make sure each character is legal, and un-escape it if possible. *)

  let characters = List.map (fun character ->

    (* Determine whether this character is legal. *)

    if not (predicate character) then
      raise (Malformed (Printf.sprintf "Character `%s' (decimal %d) is illegal in a %s."
			  (string_of_character character)
			  (code_of_character character)
			  location));

    (* If the character is an unnecessary escape sequence, convert it back to a raw character. *)

    match character with
      Escaped c when predicate (Raw c) ->
	Raw c
    | _ ->
	character

  ) characters in

  (* At this point, the list of characters is guaranteed to contain only valid characters. *)

  string_of_characters characters
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We can now define various specialized versions of the above function.

*)

let normalize_scheme string = normalize scheme "scheme" string;;
let normalize_net_loc string = normalize net_loc "network location" string;;
let normalize_query string = normalize query "query" string;;
let normalize_fragment string = normalize fragment "fragment" string;;
let normalize_param string = normalize param "parameter" string;;
let normalize_segment string = normalize segment "segment" string;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function takes a string, and escapes all characters which are invalid according to the specified predicate.
(Of course, the predicate should accept escape sequences for this to make sense, but this condition is not checked,
for efficiency.)

*)

let escape predicate string =

  (* Convert to characters. The string is a raw string, so the conversion does not look for escape sequences. *)

  let characters = characters_of_string false string in

  (* Escape all invalid characters. *)

  let characters = List.map (function
				 (Raw c) as char when not (predicate char) -> Escaped c
			       | char -> char
  ) characters in

  (* Convert back to a string. *)

  string_of_characters characters
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We can now define various specialized versions of the above function.

*)

let escape_segment string = escape segment string;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function takes a string and converts all escape sequences back to raw characters.

*)

let raw string =

  (* Convert the string to characters. The conversion does look for escape sequences. *)

  let characters = characters_of_string true string in

  (* Convert the characters into raw characters. *)

  let characters = List.map (function
			       Escaped c -> Raw c
			     | c -> c) characters in

  (* Convert back to a string. *)

  string_of_characters characters
;;

