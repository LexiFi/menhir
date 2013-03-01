(* $Header: /net/pauillac/caml/repository/bigbro/check_master.ml,v 1.5 2001/07/16 15:04:03 fpottier Exp $ *)

open Link_info
open Link_skimmer

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function is called when a check is complete, except for the fragment check. It performs the fragment check if
necessary, then passes the result to the output routines.

*)

let got_fragmentless_check_result info outcome =

  (* If fragment analysis is off, there is nothing to do, so the outcome is unmodified. Besides, if the outcome
     is a failure, we don't need to look any further, so we leave unchanged too. *)

  let outcome = if Settings.fragments && (is_successful outcome) then begin

    (* Determine whether the URL has a fragment. *)

    match Url.get_fragment info.fli_mapped_url with
      Some fragment -> (

	(* Determine whether this fragment exists. We know that the document has been analyzed, if it contains HTML. *)

	try
	  if Fragment_skimmer.exists info.fli_mapped_url fragment then outcome
	  else (OutcomeFragmentFailure fragment)
	with Not_found ->
	  OutcomeIllegalFragment

      )
    | None ->
	outcome

  end
  else
    outcome in

  (* Send the modified outcome to the output code. *)

  Link_info.got_check_result info outcome
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function is called whenever a check task is complete and its result is available. We assume that there is at
least one entry for this task in the waiting room.

*)

let got_task_outcome task outcome =

  Thread_utils.protect Cache.lock (fun () ->

    (* Store this outcome in the cache. *)

    Cache.Cache.add task outcome;

    (* Take all entries for this task out of the waiting room. There is at least one such entry. *)

    let rec loop () =
      try
	let info = Cache.Room.find task in
	Cache.Room.remove task;
	got_fragmentless_check_result info outcome;
	loop()
      with Not_found ->
	()
    in
    loop()

  ) ()
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Applying mappings to the specified (absolute) URL. A mapping is a pair of regular expression and a replacement string
(which is allowed to contain $1, $2, etc.). This should be a very flexible mechanism.

All mappings are applied to the URL, one after the other. Order is irrelevant, unless the output of some mapping
is an applicable input of some other mapping - something we don't recommend.

*)

let apply_mappings text =
  List.fold_right (fun (regexp, replacement) text ->
    Pcre.replace_first ~rex:regexp ~itempl:replacement text
  ) Settings.mappings text
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Determining whether recursion is needed. Normally, we use the regular expression stored in the settings to make this
decision, but it can be overridden by the caller.

*)

type recursion_spec =
    RecursionFollowSettings
  | RecursionDo

let should_recurse text = function
    RecursionFollowSettings -> (
      match Settings.recursion with
	None ->
	  false
      |	Some regexp ->
	  Pcre.pmatch ~rex:regexp text
    )
  | RecursionDo ->
      true
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Determining whether a link is in the check cache.

A "task" here is not just determined by a URL - it is a pair of a URL and a retrieval flag, because depending
on the value of the flag, these are two different checks. (If the retrieval flag is set, the task involves retrieving
the text and analyzing its fragments; otherwise, it only involves checking the document's existence.)

This function is called before actually starting a task - first, we check whether the same task has been done
before. There are several possibilities:

- The task has been run and the outcome is available from the cache.
  We re-use the previous outcome and return true.
- The task has been seen before but is not done.
  We add the current task to the waiting room and return true.
- The task has never been seen before.
  We add the current task to the waiting room and return false.

If the result is false, then the caller knows that the task actually has to be performed.

While checking the cache, we use the unmapped URL, because it is one that describes the "logical" document. It would
be possible for several URLs to be mapped to the same physical document, and in that case, we must check it twice,
because the base URLs will differ.

It might be possible for a given URL to be checked twice - once in non-recursive mode and once in recursive mode - but
this shouldn't happen often, if at all.

*)

let check_cached_link info retrieval =

  Thread_utils.protect Cache.lock (fun () ->

    (* Look up this task in the cache. *)

    let key = (info.fli_url, retrieval) in
    try
      let outcome = Cache.Cache.find key in

      (* It is present - we are now done with this check. *)

      got_fragmentless_check_result info outcome;
      true

    with Not_found ->

      (* Look up this task in the waiting room. If somebody's already waiting, then all we have to do is wait too - we
	 know there's a check going on. *)

      try
	let _ = Cache.Room.find key in

	(* Enter the waiting room - this task is already running. *)

	Cache.Room.add key info;
	true

      with Not_found ->

	(* Enter the waiting room, and let the caller know this task must be started. *)

	Cache.Room.add key info;
	false

  ) ()
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Checking file: links.

*)

let check_file_link info should_recurse =
  let outcome = Check_file.check info.fli_url info.fli_mapped_url should_recurse in
  got_task_outcome (info.fli_url, should_recurse) outcome
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Checking http: links.

*)

let check_network_link info should_recurse =

  (* Queue a request to deal with this URL.

     It would also be possible to wait here until a thread is available and spawn the thread right here. However,
     our choice makes the program even more responsive, since it can go on checking local links. A disadvantage is that
     the job queue might grow very quickly and use a lot of memory. *)

  (* Note that the full_link_info structure does not need to be passed as part of the job description. It has
     been saved in the waiting room, where it will be retrieved when the check is done. *)

  Agency.add (Agency.JobNetworkCheck (info.fli_url, info.fli_mapped_url, should_recurse))
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function checks a link for which a full info structure is available.

*)

let check_link info recursion_spec =

  (* As input, we expected a full_link_info structure, which contains both the unmapped and the mapped URLs.
     We must keep track of both, because the former shall be used as base URL, while the latter shall be used
     to actually retrieve the document. *)

  (* Create a text representation of the URL, to be used when checking whether the URL matches the -ignore or
     -rec patterns. The mapped URL is used; this should be more natural. *)

  let url_text = Url.print true info.fli_mapped_url in

  (* Check whether this URL should be ignored. Note that since the check is done only now, it is possible for
     a link to cause an "early error" even though it is to be ignored. I consider this a feature, since early
     errors are really user errors and can be fixed. *)

  if not (List.exists (fun rex -> Pcre.pmatch ~rex url_text) Settings.ignore) then begin

    (* Test whether recursion/retrieval shall be necessary. *)

    let should_recurse = should_recurse url_text recursion_spec in
    let retrieval = should_recurse || (Settings.fragments && (Url.get_fragment info.fli_mapped_url <> None)) in

    (* Check which scheme we have here. Unknown schemes are silently ignored. *)

    Url.dispatch_scheme (fun () ->

      (* We have a "file" URL. *)

      (* Also, we note that there is one case where a link is in the cache, yet it must be ignored. This happens when
	 the link has to be ignored (because it is local/remote and we are set to ignore local/remote links) but we have
	 seen it before as the initial link (the initial link is never ignored, so it is in the cache). In order to
	 behave properly, we must not retrieve the outcome from the cache, instead we must ignore the link the second
	 time. This means that we have to check whether the link should be ignored before checking the cache.

	 If we're told to ignore local links, there is nothing to be done. The initial link is never ignored, though,
	 because the user explicitly asked us to check it. *)

      if Settings.local || (info.fli_source = None) then
	if not (check_cached_link info retrieval) then
	  check_file_link info should_recurse

    ) (fun () ->

      (* We have a "http" URL. *)

      if Settings.remote || (info.fli_source = None) then
	if not (check_cached_link info retrieval) then
	  check_network_link info should_recurse

    ) info.fli_mapped_url

  end
  else

    (* The link should be ignored. *)

    Link_info.got_check_result info OutcomeIgnored
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function checks a link for which a full info structure is not yet available. The link can come from a piece of
HTML text or from the command line. A regular info structure is expected.

Arguments:
- A reference to the base URL. The function can update this URL if the link is a <BASE HREF> specification.
- A document source, if available. Used to create the full link info structure.
- Recursion settings. Used to force initial links to be recursively followed.
- An optional local base URL (in textual form), coming from the same tag (APPLET/OBJECT tags).
- The link info structure.

*)

exception GotEarlyError of early_error
exception IgnoreIt

let found_link base source_option recursion_settings local_base info =

  try

    (* Parse the URL. *)

    let link_url = try
      Url.create info.link_text
    with
      Url_syntax.Malformed message ->
	raise (GotEarlyError (EarlyInvalidURL message))
    | Url.UnknownScheme ->
	raise IgnoreIt in

    (* If this a new base URL (specified by the user using <BASE HREF>, remember it. *)

    if info.link_tag_type = TagBaseHref then begin

       (* It should be absolute. *)

       if not (Url.is_absolute link_url) then
	 raise (GotEarlyError EarlyBaseNotAbsolute);

       base := link_url

    end
    else begin

      (* Otherwise, it's a regular link and we should check it. First, parse the local base URL, if any,
	 and resolve it with respect to the base URL.
	 Parsing cannot fail, because the local base URL has already been checked itself. *)

      let base = match local_base with
	None ->
	  !base
      | Some local_base ->
	  let local_base = try
	    Url.create local_base
	  with Url.UnknownScheme ->
	    raise IgnoreIt in
	  Url.resolve !base local_base in

      (* Then, resolve it with respect to the current base URL (which can be the document's base URL or
         the resolved local base URL. *)

      let resolved_url = Url.resolve base link_url in

      (* Apply mappings to it. *)

      let resolved_url_text = Url.print true resolved_url in
      let mapped_url_text = apply_mappings resolved_url_text in

      (* If it is a "file:" URL, try to resolve any `~' character in it. *)

      let mapped_url_text = Sysmagic.resolve_home mapped_url_text in

      (* Parse the text back into a URL structure. *)

      let mapped_url = try
	Url.create mapped_url_text
      with
	Url_syntax.Malformed message ->
	  raise (GotEarlyError (EarlyInvalidMappedURL (resolved_url_text, mapped_url_text, message)))
      | Url.UnknownScheme ->
	  raise IgnoreIt in

      (* Create a full link info structure for this link. *)

      let fli = {
	fli_tag_type = info.link_tag_type;
        fli_line = info.link_line;
	fli_column = info.link_column;
	fli_url_text = info.link_text;
	fli_url = resolved_url;
	fli_mapped_url = mapped_url;
	fli_name = info.link_name;
	fli_source = source_option
      } in

      (* Check it. *)

      check_link fli recursion_settings

    end

  with
    GotEarlyError error ->
      Link_info.got_early_error info source_option error
  | IgnoreIt ->
      ()
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Parsing a piece of HTML text and taking appropriate action.

*)

let check_html data url source =

  (* Initialize the base URL as the (unmapped) document URL. It might change during the process. *)

  let base = ref url in

  (* Whenever a content type specification is found, update the source with it. *)

  let found_content_type content_type =
    try
      let media_type = Media_type.create content_type in
      source.source_charset <- Media_type.charset media_type
    with Media_type.ParseError ->
      ()
  in

  (* Go ahead. *)

  skim {
    found_link = found_link base (Some source) RecursionFollowSettings;
    found_content_type = found_content_type
  } data
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Checking an initial link.

*)

let wd_url =
  Url.of_cwd()
;;

let check_initial_link url_text =

  (* Build a link info structure for this link. *)

  let info = {
    link_tag_type = TagInitial;
    link_line = 0;
    link_column = 0;
    link_text = url_text;
    link_name = None
  } in

  (* The base URL is the directory URL. This allows relative Unix filenames to have their expected meaning. *)

  let base = ref wd_url in

  (* Go ahead. *)

  found_link base None RecursionDo None info
;;
