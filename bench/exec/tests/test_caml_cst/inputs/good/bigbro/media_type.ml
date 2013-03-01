(* $Header: /net/pauillac/caml/repository/bigbro/media_type.ml,v 1.1.1.1 2001/02/13 15:39:34 fpottier Exp $ *)

open String_utils

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This type describes a media type field, as defined by the HTTP/1.1 protocol.
A media type contains a type, a subtype, and a list of (attribute, value) pairs.

*)

type media_type = string * string * ((string * string) list)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Exceptions.

*)

exception ParseError

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Auxiliary parsing functions.

*)

let is_tspecial = function
    '('
  | ')'
  | '<'
  | '>'
  | '@'
  | ','
  | ';'
  | ':'
  | '\\'
  | '"'
  | '/'
  | '['
  | ']'
  | '?'
  | '='
  | '{'
  | '}'
  | ' '
  | '\t' ->
      true
  | _ ->
      false
;;

let is_control c =
  let code = Char.code c in
  (code < 0x20) or (code = 0x7F)
;;

let is_text c =
  not (is_control c)
;;

let is_token c = 
  not ((is_control c) or (is_tspecial c))
;;

let is_ws = function
    '\t'
  | '\n'
  | '\r'
  | ' ' ->
      true
  | _ ->
      false
;;

let skip_ws text =
  let _, text = SubstringOp.filter is_ws text in
  text
;;

let get_token text =

  (* Read one token. *)

  let (token, text) as result = SubstringOp.filter is_token text in

  (* If it is empty, we have an error. *)

  if Substring.length token = 0 then
    raise ParseError;

  result
;;

let get_token_or_quoted_string text =

  (* Check whether we have a double quote. If there is no text, it is an error. *)

  let quote_present = try
    Substring.get text 0 = '"'
  with Invalid_argument _ ->
    raise ParseError in

  if quote_present then begin

    (* Skip the opening quote. *)

    let text = Substring.sub text 1 (Substring.length text - 1) in

    (* Look for the closing quote. *)

    let between, _, text = try
      SubstringOp.chop (fun c -> c = '"') text
    with Not_found ->
      raise ParseError in

    between, text

  end
  else
    get_token text
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This utility makes sure that the supplied substring starts with the expected character, and cuts it off.

*)

let assert_leading_char text c =

  let text = skip_ws text in
  try
    if Substring.get text 0 <> c then
      raise ParseError;
    Substring.sub text 1 (Substring.length text - 1)
  with Invalid_argument _ ->
    raise ParseError
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Parsing parameters.

*)

let rec create_params text =

  (* Skip leading whitespace. If nothing remains, we're done. *)

  let text = skip_ws text in
  if (Substring.length text = 0) then
    []
  else begin

    (* The parameter must start with a semi-colon. *)

    let text = assert_leading_char text ';' in
  
    (* Read the attribute name. *)

    let text = skip_ws text in
    let attribute, text = get_token text in

    (* It must be followed by an equal sign. *)

    let text = assert_leading_char text '=' in

    (* Read the attribute value. *)

    let text = skip_ws text in
    let value, text = get_token_or_quoted_string text in

    (* Read any remaining parameters and append the pair we just parsed. *)

    let params = create_params text in
    (String.lowercase (Substring.to_string attribute), Substring.to_string value) :: params

  end
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main parsing function.

*)

let create text =

  let text = Substring.of_string text in

  (* Read the media type. *)

  let text = skip_ws text in
  let mtype, text = get_token text in

  (* It must be followed by a slash. *)

  let text = assert_leading_char text '/' in

  (* Read the media subtype. *)

  let text = skip_ws text in
  let msubtype, text = get_token text in

  (* Read the parameters. *)

  let params = create_params text in

  (* Create the result. *)

  (String.lowercase (Substring.to_string mtype), String.lowercase (Substring.to_string msubtype), params)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A list of known character sets.

*)

type charset =
    CharsetISO_8859_1
  | CharsetISO_2022_JP
  | CharsetEUC_JP
  | CharsetSJIS

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The first function determines whether this media type is text/html (with possible additional parameters).

The second one returns the character set specified by this media type. It should only be called if the main type
is text. If the media type doesn't explicitly specify a charset, or if it specifies an unknown one,
ISO-8859-1 is assumed.

*)

let is_text_html (mtype, msubtype, _) =
  (mtype = "text") & (msubtype = "html")
;;

let charset (_, _, params) =

  (* Check the parameters to see if one of them defines a charset. If so, check which one. If we don't recognize
     it, ignore it. *)

  try
    match String.uppercase (List.assoc "charset" params) with
      "ISO-8859-1" ->
	CharsetISO_8859_1
    | "ISO-2022-JP" ->
	CharsetISO_2022_JP
    | "X-EUC-JP" ->
	CharsetEUC_JP
    | "X-SJIS" ->
	CharsetSJIS
    | _ ->
	raise Not_found
  with Not_found ->
    
    (* According to HTTP/1.1 (RFC 2068) :
       "When no explicit charset parameter is provided by the sender, media subtypes of the "text" type
       are defined to have a default charset value of "ISO-8859-1" when received via HTTP." *)

    CharsetISO_8859_1
;;
