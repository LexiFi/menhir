(* $Header: /net/pauillac/caml/repository/bigbro/link_skimmer.ml,v 1.2 2001/03/09 14:49:29 fpottier Exp $ *)

open Link_info
open Skimmer
open String_utils

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This structure defines the callbacks which must be supplied by our client.

*)

type callbacks = {
  found_link: string option -> link_info -> unit;
  found_content_type: string -> unit
} 

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Some storage is needed to deal with named tags.

*)

type data = {
  mutable name_start: int;				(* The start of the link's name, for <A> links only. *)
  mutable stored_link_info: link_info option		(* Info about an <A> link, stored while awaiting </A>. *)
} 

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Associating a (tag, attribute) name with its internal representation. This is the list of unnamed links of type
%URL. The only named link is A HREF and is handled separately.

We enter the data into a list of lists, but it is converted to a map of maps at startup time for speed.

Tricky tags are those which contain an attribute of type %URL which is to be considered as a base URL for some of the
other attributes.

*)

let rec convert accu = function
    [] ->
      accu
  | (tag_name, attr_list) :: rest ->

      (* Turn the secondary list into a map from strings (attribute names) to symbols. *)

      let attr_map = List.fold_right (fun (attr_name, symbol) accu ->
	StringMap.add attr_name symbol accu
      ) attr_list StringMap.empty in

      (* Add it to the main map from strings (tag names) to secondary maps. *)

      convert (StringMap.add tag_name attr_map accu) rest
;;

let tag_map =
  convert StringMap.empty [
    "AREA", ["HREF", TagAreaHref];
    "BASE", ["HREF", TagBaseHref];
    "BLOCKQUOTE", ["CITE", TagBlockquoteCite];
    "BODY", ["BACKGROUND", TagBodyBackground];
    "DEL", ["CITE", TagDelCite];
    "DIV", ["HREF", TagDivHref];
    "FORM", ["ACTION", TagFormAction];
    "FRAME", ["LONGDESC", TagFrameLongdesc; "SRC", TagFrameSrc];
    "HEAD", ["PROFILE", TagHeadProfile; "SRC", TagIframeSrc];
    "IFRAME", ["LONGDESC", TagIframeLongdesc];
    "IMG", ["LONGDESC", TagImgLongdesc; "SRC", TagImgSrc; "USEMAP", TagImgUsemap];
    "INPUT", ["SRC", TagInputSrc; "USEMAP", TagInputUsemap];
    "INS", ["CITE", TagInsCite];
    "LINK", ["HREF", TagLinkHref];
    "OBJECT", ["USEMAP", TagObjectUsemap];
    "Q", ["CITE", TagQCite];
    "SCRIPT", ["SRC", TagScriptSrc];
    "SPAN", ["HREF", TagSpanHref];
  ];;

let tricky_tags = [
  "APPLET", "CODEBASE", TagAppletCodebase,
    ["CODE", TagAppletCode; "ARCHIVE", TagAppletArchive; "OBJECT", TagAppletObject];
  "OBJECT", "CODEBASE", TagObjectCodebase,
    ["CLASSID", TagObjectClassid; "DATA", TagObjectData; "ARCHIVE", TagObjectArchive]
];;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Taking action when full information about a tag and its attributes is available.

*)

let handle_tag callbacks data tag_info =

  (* Determine the tag name. *)

  let tag_name = String.uppercase
		   (String.sub tag_info.ti_text tag_info.ti_tag_start (tag_info.ti_tag_end - tag_info.ti_tag_start)) in

  (* A utility. It determines whether a given attribute exists in our tag, and if so, reports it as a side effect.
     It returns the attribute's textual value, if present. *)

  let attr_exists local_base known_attr_name known_attr_symbol =
    try
      match StringMap.find known_attr_name tag_info.ti_tag_attr with
	Some (value_line, value_column, value_text) ->
	  
          (* Create a link info structure and pass it to found_link. *)

	  callbacks.found_link local_base {
	    link_tag_type = known_attr_symbol;
	    link_line = value_line;
	    link_column = value_column;
	    link_text = value_text;
	    link_name = None
	  };
	  Some value_text
      | None ->
	  None
    with Not_found ->
      None in

  (* Check whether we have a (tag, attribute) pair among the known ones. First, look up the tag name in the
     table. *)

  Misc.do_if_found (fun () ->
    StringMap.iter (fun known_attr_name known_attr_symbol ->
      let _ = attr_exists None known_attr_name known_attr_symbol in
      ()
    ) (StringMap.find tag_name tag_map)
  );

  (* In addition, check whether this is a tricky tag. *)

  List.iter (fun (tricky_tag_name, master_attr_name, master_attr_symbol, other_attrs) ->
    if tricky_tag_name = tag_name then begin

      (* Check whether the master attribute (i.e. the one which contains the base URL) is present. If so, report it
	 and extract its value. *)

      let local_base = attr_exists None master_attr_name master_attr_symbol in

      (* Now, look for the secondary attributes. *)

      List.iter (fun (other_attr_name, other_attr_symbol) ->
	let _ = attr_exists local_base other_attr_name other_attr_symbol in
	()
      ) other_attrs

    end
  ) tricky_tags;

  (* Check whether this is an <A HREF> tag (i.e. a named link). *)

  if (tag_name = "A") then
    Misc.do_if_found (fun () ->
      let attr_value = StringMap.find "HREF" tag_info.ti_tag_attr in
      Misc.do_option attr_value (fun (value_line, value_column, value_text) ->
		
	(* Create a temporary link info structure. Everything is known but the name, which hasn't been read
	   yet. We shall store this structure until the </A> tag is found. *)

	  let link_info = {
	  link_tag_type = TagAHref;
	  link_line = value_line;
          link_column = value_column;
	  link_text = value_text;
	  link_name = None
	} in

	data.name_start <- succ tag_info.ti_index;
	data.stored_link_info <- Some link_info
      )
    );

  (* Check whether this is a </A> tag (i.e. the end of a named link's name). *)

  if (tag_name = "/A") then
    Misc.do_option data.stored_link_info (fun link_info ->

      (* We now know where the last link's name ends. *)

      callbacks.found_link None {
	link_tag_type = TagAHref;
        link_line = link_info.link_line;
        link_column = link_info.link_column;
	link_text = link_info.link_text;
	link_name = Stripper.strip true
		      (String.sub tag_info.ti_text data.name_start (tag_info.ti_tag_start - 1 - data.name_start))
      };
      data.stored_link_info <- None
    );
    
  (* Check whether this is a <META HTTP-EQUIV> tag which defines a Content-Type HTTP header. *)

  if (tag_name = "META") then
    Misc.do_if_found (fun () ->
      let http_equiv_attr_value = StringMap.find "HTTP-EQUIV" tag_info.ti_tag_attr in
      Misc.do_option http_equiv_attr_value (fun (_, _, value_text) ->
	if value_text = "CONTENT-TYPE" then
	  let content_attr_value = StringMap.find "CONTENT" tag_info.ti_tag_attr in
	  Misc.do_option content_attr_value (fun (_, _, value_text) ->
	    callbacks.found_content_type value_text
	  )
      )
    )
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Skim parses a piece of HTML text and calls the hooks when necessary.

*)

let skim callbacks text = 

  (* Create the initial state. *)

  let data = {
    stored_link_info = None;
    name_start = 0
  } in

  (* Parse the document. *)

  Skimmer.skim (handle_tag callbacks data) text

;;
