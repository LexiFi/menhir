(* $Header: /net/pauillac/caml/repository/bigbro/check_http.ml,v 1.1.1.1 2001/02/13 15:39:36 fpottier Exp $ *)

open Http_connection
open Link_info
open Media_type

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A first attempt at the main function. Here, we do not attempt to catch exceptions thrown by Http_connection.
They are dealt with outside.

*)

let check url mapped_url should_recurse =

  (* Determine under which conditions fragment analysis shall be performed. (See Check_file for more comments.) *)

  let will_do_fragments_if_read = Settings.fragments & (not (Fragment_skimmer.analyzed mapped_url)) in
  let must_do_fragments = will_do_fragments_if_read & (Url.get_fragment mapped_url <> None) in

  (* Indicate how timeouts should be handled by the core HTTP code. *)

  let timeout_handler () = Check_master.got_task_outcome (url, should_recurse) OutcomeTimeout in

  (* Issue a HEAD request to the HTTP server. *)

  Stats.did_http_head();

  let answer = Http_connection.request mapped_url Url.HEAD timeout_handler in

  (* Create a document source structure for this document. It shall be used for recursion.
     Right now, we set the character set to a default value, and the code below updates if it finds a charset
     specification in the server's answer. *)

  let source = {
    source_url = Url.drop_fragment mapped_url;
    source_charset = CharsetISO_8859_1
  } in

  (* Have a look at the result code. *)

  let outcome, available_html = match answer.http_response_code with
    301 | 302 -> (

      (* The document has moved. We must determine its new URL, which is specified by the Location header. *)

      try
      	let new_location = StringMap.find "location" answer.http_other_headers in

	(* Some servers specify a URI as the new location, instead of a full URL. To work around this problem,
	   we try resolving the new location with respect to the previous URL. This will have the effect of
	   restoring the protocol and server name, if they are missing. Of course, if the new location can't
	   be parsed, we abandon. *)

	let new_location = try
	  Url.print true (Url.resolve mapped_url (Url.create new_location))
	with
	  Url_syntax.Malformed _
	| Url.UnknownScheme ->
	    new_location in

	OutcomeMoved (answer.http_response_code = 301, new_location), false
      with Not_found ->
	OutcomeRemoteFailure HTTPErrorProtocolViolation, false

    )
  | 200 -> (

      (* The connection was successful. Determine whether the document contains HTML text. *)

      match answer.http_content_type with
	Some media_type when Media_type.is_text_html media_type ->
	  source.source_charset <- Media_type.charset media_type;
	  OutcomeRemoteSuccess, true
      | _ ->
	  OutcomeRemoteSuccess, false
      
    )
  | _ ->
      
      (* The connection failed. *)

      OutcomeServerFailure answer.http_response_code, false

  in

  (* If either recursion of fragment analysis is necessary, and if the document is an HTML document, then we need
     to download it, by making a GET request to the server. *)

  let outcome = if (should_recurse or must_do_fragments) & available_html then begin

    (* Issue a GET request. *)

    Stats.did_http_get();

    let answer = Http_connection.request mapped_url Url.GET timeout_handler in

    (* Analyze the answer. *)

    match (answer.http_response_code, answer.http_body) with
      200, Some data ->

	Stats.http_size_was (String.length data);

	(* We got the data. Now that the document is available, analyze its fragments. *)

	if will_do_fragments_if_read then
	  Fragment_skimmer.analyze mapped_url data;

	(* If recursion was requested, queue a recursion request. *)

	if should_recurse then
	  Agency.add (Agency.JobRecurse (data, Url.drop_fragment url, source));

	outcome

    | _ ->
	
	OutcomeServerFailure answer.http_response_code

  end
  else outcome in

  outcome
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Dealing with connection errors.

*)

let check url mapped_url should_recurse =
  try
    check url mapped_url should_recurse
  with HTTPException http_error ->
    OutcomeRemoteFailure http_error
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Completing the check.

*)

let check url mapped_url should_recurse =
  let outcome = check url mapped_url should_recurse in
  Check_master.got_task_outcome (url, should_recurse) outcome
;;

    
