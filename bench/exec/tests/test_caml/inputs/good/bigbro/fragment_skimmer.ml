(* $Header: /net/pauillac/caml/repository/bigbro/fragment_skimmer.ml,v 1.1.1.1 2001/02/13 15:39:37 fpottier Exp $ *)

open Skimmer

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Define sets of strings.

*)

module StringSet = Set.Make (struct
			       type t = string
			       let compare = Pervasives.compare
			     end)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Define what to do when a tag is found.

*)

let handle_tag accu tag_info =

  (* Determine the tag name. *)

  let tag_name = String.uppercase
		   (String.sub tag_info.ti_text tag_info.ti_tag_start (tag_info.ti_tag_end - tag_info.ti_tag_start)) in

  (* Define an auxiliary function to look for a fragment name in a specified attribute, if present. *)

  let get_fragment attr_name =
    Misc.do_if_found (fun () ->
      Misc.do_option (StringMap.find attr_name tag_info.ti_tag_attr) (fun (_, _, value_text) ->
       accu := StringSet.add value_text !accu
      )
    ) in

  (* Determine whether we have a <A NAME> or a <MAP NAME> tag. There might be other places where fragments are defined,
     but the HTML 4.0 specification isn't very clear about it. We'll see if users complain. *)

  if (tag_name = "A") or (tag_name = "MAP") then
    get_fragment "NAME";

  (* Determine we have an ID attribute, regardless of the tag name. *)

  get_fragment "ID"

;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function parses a piece of text and returns the set of fragments defined in it.

*)

let skim text = 
  let accu = ref StringSet.empty in
  Skimmer.skim (handle_tag accu) text;
  !accu
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Now, define the internal association table. It would have been possible to separate the parsing code from the
association table code, but we obtain a more compact interface by grouping them.

*)

module UrlMap = Map.Make (struct
			    type t = Url.url
			    let compare = Pervasives.compare
			  end)

let lock = Mutex.create()
and table = ref UrlMap.empty

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The three entry points.

*)

let normalize url =
  Url.drop_fragment url
;;

let analyze url text =
  let url = normalize url in
  let fragments = skim text in
  Thread_utils.protect lock (fun () ->
    table := UrlMap.add url fragments !table
  ) ()
;;

let analyzed url =
  let url = normalize url in
  Thread_utils.protect lock (fun () ->
    try
      let _ = UrlMap.find url !table in
      true
    with Not_found ->
      false
  ) ()
;;

let exists url fragment =
  let url = normalize url in
  Thread_utils.protect lock (fun () ->
    let fragments = UrlMap.find url !table in
    StringSet.mem fragment fragments
  ) ()
;;
