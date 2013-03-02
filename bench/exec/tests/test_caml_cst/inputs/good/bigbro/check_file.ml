(* $Header: /net/pauillac/caml/repository/bigbro/check_file.ml,v 1.3 2001/08/01 11:43:54 fpottier Exp $ *)

open Link_info
open Media_type
open Url
open Unix

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

For simplicity, we use an exception to report results internally.

*)

exception GotOutcome of outcome

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Turn the index file name into a relative URL. This is done once at startup to save time.

*)

let index_url =
  Url.create Settings.index
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Checking file: links.

*)

let check url mapped_url should_recurse =

  (* Convert the URL to a local file name. *)

  let filename = Url.filename mapped_url in

  (* Check whether fragment analysis is necessary.

     There are several points to understand here. First, we are not concerned here with checking this specific URL's
     fragment. We are only interested in performing the document's fragment analysis if necessary. The actual check
     for this fragment shall be done later.

     Second, the fragment analysis is considered necessary only if the -fragments flag is set, and this URL carries
     a fragment, and this file hasn't been analyzed until now.

     Finally, one should note that if recursion is performed, then we will perform fragment analysis, even if the
     latter was found to be unnecessary. This ensures that files are only read/downloaded once. (If we did not do
     this, then we might read the file once to do recursion on a fragment-less URL, and later have to read it again
     because we find a fragment-carrying URL.) *)

  let will_do_fragments_if_read = Settings.fragments & (not (Fragment_skimmer.analyzed mapped_url)) in
  let must_do_fragments = will_do_fragments_if_read & (Url.get_fragment mapped_url <> None) in

  (* Check whether the file exists. *)

  Stats.did_file_head();

  let stats = try
    stat filename
  with Unix_error _ ->
    raise (GotOutcome (OutcomeLocalFailure LFailNoSuchFile)) in

  (* If this is a directory, we must check whether it contains an index file. Regardless of the result, the link
     is valid - however, if there is an index file, then recursion becomes possible. *)

  let filename, recursion_data = if stats.st_kind = S_DIR then begin

    (* Since the URL points to a directory, it should end with a slash. *)

    if not (Url.is_directory url) then
      raise (GotOutcome (OutcomeLocalFailure LFailNoTrailingSlash));

    (* Check whether an index file exists. *)

    let indexfilename = Filename.concat filename Settings.index in
    if Sys.file_exists indexfilename then

      (* If so, create the corresponding (unmapped & mapped) URLs. Note that by using Url.resolve like this,
	 we drop the fragment, which is desired. *)

      indexfilename, Some (Url.resolve url index_url, Url.resolve mapped_url index_url)

    else
      filename, None

  end
  else

    (* Recursion takes place on the whole document, not on a part of it, so we must remove the fragment before
       passing the URL. *)

    filename, Some (Url.drop_fragment url, Url.drop_fragment mapped_url) in

  (* If either recursion or fragment analysis is necessary, and if the file is an HTML file (which we determine
     by looking at its name), then we need to read it. *)

  if (((recursion_data <> None) & should_recurse) or must_do_fragments)
   & (Pcre.pmatch ~rex:Settings.local_html_files (Filename.basename filename)) then begin

    (* The file is an HTML file. Read it. *)

    Stats.did_file_get();

    let data = try
      Io_utils.read_whole_file filename
    with Sys_error _ ->
      raise (GotOutcome (OutcomeLocalFailure LFailUnreadable)) in

    Stats.file_size_was (String.length data);

    (* Now that the file is available, analyze its fragments, even if that was not necessary. *)

    if will_do_fragments_if_read then
      Fragment_skimmer.analyze mapped_url data;

    (* If recursion was requested, do it. *)

    Misc.do_option recursion_data (fun (recurse_url, mapped_recurse_url) ->
      if should_recurse then begin

        (* Create a document source structure. *)

      	let source = {
	  source_url = mapped_recurse_url;
	  source_charset = CharsetISO_8859_1
      	} in

        (* Queue a request to parse and check this file. *)

      	Agency.add (Agency.JobRecurse (data, recurse_url, source))

      end
    )

  end;

  (* The check is a success. Recursion is not taken into account here, since it considered as a set of further
     checks. Note that we haven't checked yet whether this specific URL's fragment is correct; we shall do so
     later. *)

  OutcomeLocalSuccess
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Wrap the above function into a handler for our internal exception.

*)

let check url mapped_url should_recurse =
  try
    check url mapped_url should_recurse
  with GotOutcome outcome ->
    outcome
;;
