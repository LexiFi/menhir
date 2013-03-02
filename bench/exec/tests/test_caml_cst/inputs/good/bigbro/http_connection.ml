(* $Header: /net/pauillac/caml/repository/bigbro/http_connection.ml,v 1.1.1.1 2001/02/13 15:39:38 fpottier Exp $ *)

open Linear_connection
open String_utils

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Exceptions.

*)

type http_error =
    HTTPErrorConnection of connection_action * Unix.error
  | HTTPErrorProtocolViolation
  | HTTPErrorEmptyAnswer
  | HTTPErrorIncompleteData of int * int

exception HTTPException of http_error

let error http_error =
  raise (HTTPException http_error)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Apply the Map functor to create the dictionary.

*)

module StringMap = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Copy the type declaration.

*)

type http_answer = {
    http_response_code: int;
    
    http_content_length: int option;
    http_content_type: Media_type.media_type option;
    http_other_headers: string StringMap.t;

    http_body: string option
  } 

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Looking for an end-of-line. This function returns the piece of text which precedes the next EOL marker, and the
piece of text which follows it. If no EOL marker exists, the second piece is empty.

The HTTP protocol specifies that the end of line marker for headers is CRLF but recommends recognizing a
lone LF; indeed some servers, such as Apache, generate LF only.

*)

let next_line buffer = 
  
  (* Look for a LF character. *)

  try
    let line, _, rest = SubstringOp.chop (fun c -> (c = '\n')) buffer in

    (* Check whether there was a CR in front of it. *)

    let line_length = Substring.length line in
    if (line_length > 0) & (Substring.get line (line_length - 1) = '\r') then
      Substring.sub line 0 (line_length - 1), rest
    else
      line, rest

  with Not_found ->

    (* If no LF was found, this is the last line. *)

    buffer, Substring.sub buffer 0 0
;;  

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A simple utility to detect blanks.

*)

let is_blank c =
  (c = ' ') or (c = '\t')
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Parsing an HTTP answer.

*)

let parse_answer request_type buffer =

  let buffer = Substring.of_string buffer in

  (* We have a special message when the answer is empty. *)

  if Substring.length buffer = 0 then
    error HTTPErrorEmptyAnswer;

  (* Make sure the buffer begins with the HTTP/ keyword. *)

  let buffer = try
    let keyword, buffer = SubstringOp.split buffer 5 in
    if (Substring.to_string keyword) <> "HTTP/" then
      error HTTPErrorProtocolViolation;
    buffer
  with Invalid_argument _ ->
    error HTTPErrorProtocolViolation in

  (* Skip the HTTP protocol version number. It had better be compatible with ours. *)

  let _, _, buffer = try
    SubstringOp.chop (fun c -> c = ' ') buffer
  with Not_found ->
    error HTTPErrorProtocolViolation in

  (* Read the three-digit response code. *)

  let response_code, buffer = try
    let response_code, buffer = SubstringOp.split buffer 3 in
    int_of_string (Substring.to_string response_code), buffer
  with
    Invalid_argument _
  | Failure _ ->
    error HTTPErrorProtocolViolation in

  (* Skip to the next line. *)

  let _, buffer = next_line buffer in

  (* Loop over the headers.
     According to RFC 2068: "Field names are case-insensitive. The field value may be preceded by any amount
     of LWS, though a single SP is preferred. Header fields can be extended over multiple lines by preceding
     each extra line with at least one SP or HT." *)

  let rec loop ((content_length, content_type, dictionary) as accu) buffer =

    (* We are currently positioned at the beginning of a new header. Look for its end - it might have several lines.
       If it does have several lines, we drop the EOL markers and concatenate the lines. *)

    let rec read_one_header header buffer =
      let line, buffer = next_line buffer in
      let header = header ^ (Substring.to_string line) in
      if (Substring.length buffer > 0) & (is_blank (Substring.get buffer 0)) then
	read_one_header header buffer
      else
	header, buffer in

    let header, buffer = read_one_header "" buffer in

    (* If this header is empty, we have reached the end of the header area. *)

    if String.length header = 0 then accu, buffer
    else begin

      (* Separate the header's name from its value by looking for the first colon. *)

      let header_name, _, header_value = try
	StringOp.chop (fun c -> c = ':') header
      with Not_found ->
	error HTTPErrorProtocolViolation in

      (* Normalize the header name with respect to case. *)

      let header_name = String.lowercase header_name in

      (* Ignore white space at the beginning of the header value. *)

      let _, header_value = StringOp.filter is_blank header_value in

      (* There are a few particular fields which are known to us. For each of these fields, we know what the
	 value represents, so we can convert it from a string to a more suitable type of object. *)

      let accu = match header_name with
	"content-length" -> (
	    
	  try
	    (Some (int_of_string header_value), content_type, dictionary)
	  with Failure _ ->
	    error HTTPErrorProtocolViolation

        )
      | "content-type" -> (

	  try
	    (content_length, Some (Media_type.create header_value), dictionary)
	  with Media_type.ParseError ->
	    error HTTPErrorProtocolViolation

        )
      | _ ->

	  (content_length, content_type, StringMap.add header_name header_value dictionary) in

      (* Continue. *)

      loop accu buffer

    end in

  (* Do it. *)

  let (content_length, content_type, dictionary), buffer = loop (None, None, StringMap.empty) buffer in

  (* We are now at the beginning of the body, if there is one, or at the end of the answer, otherwise. *)

  let body_length = Substring.length buffer in
  let body = if body_length > 0 then begin

    (* If a Content-Length field was supplied by the server, check it against the body's length.
       This should help detect aborted transfers. If, on the other hand, the server sent too much data,
       we ignore the problem. Of course, this test makes sense only for GET requests. *)

    match content_length with
      Some content_length when (request_type = Url.GET) & (content_length > body_length) ->
	error (HTTPErrorIncompleteData (content_length, body_length))
    | _ ->
	Some (Substring.to_string buffer)

  end
  else
    None in

  (* We are now done collecting everything. *)

  {
    http_response_code = response_code;
    http_content_length = content_length;
    http_content_type = content_type;
    http_other_headers = dictionary;
    http_body = body
  }
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Implementing gentleness. Resolution is around 1 second.

*)

module Gentle = struct

  (* This table stores the last access time of each server. *)

  let table = ref (StringMap.empty : float StringMap.t)
  and lock = Mutex.create()

  (* Call this function when preparing to hit a server. It waits an appropriate amount of time. *)

  let hit minimum server =

    let delay = Thread_utils.protect lock (fun () ->

      let now = Unix.time() in
      try

        (* Find out when this server was last hit. *)

        let last_hit = StringMap.find server !table in

        (* Compute the new minimum hit time, and decide whether we should wait. *)

        let next_hit_limit = last_hit +. minimum in
        let next_hit = max now next_hit_limit in
        
        (* Record the new hit time. This has to be done now, not after we have waited, because we must release
	   the lock as soon as possible. *)

        table := StringMap.add server next_hit !table;

        (* Return the computed delay. *)

        next_hit -. now

     with Not_found ->

        (* The server was never hit. *)

        table := StringMap.add server now !table;
        0.0

    ) () in

    (* If necessary, wait. *)

    if delay > 0.0 then
      Thread.delay delay
  ;;

  (* At initialization time, determine whether we have to be gentle. If not, the code above is useless, and we can
     save time and memory by replacing it with a dummy function. *)

  let hit = match Settings.gentle with
    Some minimum ->
      hit minimum
  | None ->
      (fun _ -> ())
  ;;

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Gluing everything together.

*)

let request url request_type timeout_handler =
  try
    let text, host, port = Url.create_http_request request_type url in
    Gentle.hit host;
    parse_answer request_type (Linear_connection.request text host port timeout_handler)
  with
    Misc.InvalidPort _ ->
      error HTTPErrorProtocolViolation
  | ConnectionError(action, err) ->
      error (HTTPErrorConnection(action, err))
;;
