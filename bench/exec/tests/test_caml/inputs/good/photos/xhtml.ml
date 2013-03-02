(* $Header: /home/yquem/cristal/fpottier/cvs/photos/xhtml.ml,v 1.6 2005/12/11 21:36:47 fpottier Exp $ *)

open Printf
open Database
open Util

(** Printing generic text. *)
let print text channel =
  output_string channel text

(** Creating a generic tag. *)
let tag name attributes content channel =
   fprintf channel "<%s %s>%t</%s>" name attributes content name

(** Creating a generic empty tag. *)
let emptytag name attributes channel =
   fprintf channel "<%s %s />" name attributes

(** Creating a tr tag. *)
let tr content channel =
  tag "tr" "" content channel

(** Creating a td tag (with centered content). *)
let td content channel =
  tag "td" "align=\"center\"" content channel

(** Creating a single-column table out of a list. *)
let single_column_table attributes contents channel =
  tag "table" attributes (fun channel ->
    List.iter (fun content ->
      tr (td content) channel
    ) contents
  ) channel

(** Creating a single-row table out of a list. *)
let single_row_table attributes contents channel =
  tag "table" attributes (fun channel ->
    tr (fun channel ->
      List.iter (fun content ->
        td content channel
      ) contents
    ) channel
  ) channel

(** Creating a multi-column table out of a list. The [renderer] function
    is invoked to create the table elements. *)
let table attributes width elements renderer channel =
  tag "table" attributes (fun channel ->
    let i = ref 0 in
    let n = List.length elements in
    List.iter (fun element ->
      if !i mod width = 0 then
	fprintf channel "<tr>";
      td (renderer element) channel;
      if ((!i+1) mod width = 0) || (!i+1 = n) then
	fprintf channel "</tr>";
      incr i
    ) elements
  ) channel

(** Creating an image tag. *)
let image filename link alt channel =
  let attributes =
    match Image.resolution filename with
    | None ->
        ""
    | Some (width, height) ->
        sprintf "width=\"%d\" height=\"%d\" " width height
  in
  let attributes =
    sprintf "src=\"%s\" alt=\"%s\" %s" link alt attributes
  in
  emptytag "img" attributes channel

(** Creating a hyperlink tag. *)
let hlink link content channel =
  tag "a" (sprintf "href=\"%s\"" link) content channel

(** Creating an anchor tag. *)
let anchor name content channel =
  tag "a" (sprintf "name=\"%s\"" name) content channel

(** Printing a date. *)
let months =
  [| "janvier"; "février"; "mars"; "avril"; "mai"; "juin";
     "juillet"; "aout"; "septembre"; "octobre"; "novembre"; "décembre" |]

let wdays =
  [| "Dimanche"; "Lundi"; "Mardi"; "Mercredi"; "Jeudi"; "Vendredi"; "Samedi" |]

let mkdate date =
  let tm = Unix.localtime date in
  let année = 1900 + tm.Unix.tm_year in
  let mois = months.(tm.Unix.tm_mon) in
  let jour = tm.Unix.tm_mday in
  let heure = tm.Unix.tm_hour in
  let minute = tm.Unix.tm_min in
  let seconde = tm.Unix.tm_sec in
  let jsemaine = wdays.(tm.Unix.tm_wday) in
  sprintf "%s %d %s %d à %d:%02d:%02d" jsemaine jour mois année heure minute seconde

(** Creating an entire XHTML file. *)
let make filename title style content =
  let oc = open_out filename in
  let out = output_string oc in
  out "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n";
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n";
  out "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"fr\" lang=\"fr\">\n";
  out "<head><style type=\"text/css\">";
  out style;
  out "</style><title>";
  out title;
  out "</title></head><body>\n\n";

  content oc;

  out "\n\n<p><a href=\"http://validator.w3.org/check/referer\"><img src=\"http://www.w3.org/Icons/valid-xhtml10\" alt=\"Valid XHTML 1.0!\" /></a></p></body></html>\n";
  close_out oc

module Make (X : sig

  (** Elements to be included in the album. *)
  val elements: string list

  (** Album title. *)
  val title: string

  (** Album subdirectory. *)
  val subdirectory: string

  (** Progress function. *)
  val pulse: unit -> unit

end) = struct

  open X

  (** Sort the list of elements. *)
  let elements =
    sort_by_date elements

  (** Determine where the album will be stored. *)
  let gallerydir =
    Config.albums ^ subdirectory ^ "/"

  (** Wipe out any previous version of the album. *)
  let _ =
    command1 "/bin/rm -rf" gallerydir;
    command1 "mkdir" gallerydir;
    command1 "mkdir" (gallerydir ^ "Reduced/");
    command1 "mkdir" (gallerydir ^ "Thumbnails/")

  let page element =
    gallerydir ^ (Filename.chop_extension element) ^ ".html"

  (** How thumbnails are referenced from the main page. *)
  let thumbnail_link_of_element element =
    "Thumbnails/" ^ element

  (* We create local copies of the [Reduced] and [Thumbnails] files in the album's
     directory, since this makes copying the directory as a whole easier. Ideally,
     we could create these copies using [ln], but that won't work on a DOS filesystem,
     which is the one I use for Windows compatibility. *)

  (** Creating a local copy of a thumbnail image. *)
  let thumbnail element =
    let srcname = thumbnail element
    and dstname = gallerydir ^ (thumbnail_link_of_element element) in
    Util.command (sprintf "/bin/cp %s %s" (Filename.quote srcname) (Filename.quote dstname));
    dstname

  (** How a reduced image is referenced from its image page. *)
  let reduced_link_of_element element =
    "Reduced/" ^ element

  (** Creating a local copy of a reduced image. *)
  let reduced element =
    let srcname = reduced element
    and dstname = gallerydir ^ (reduced_link_of_element element) in
    Util.command (sprintf "/bin/cp %s %s" (Filename.quote srcname) (Filename.quote dstname));
    dstname

  (** How an image page is referenced from another page. *)
  let page_link_of_element element =
    (Filename.chop_extension element) ^ ".html"

  (** The width of the table where images are laid out in the main page. *)
  let gallery_width =
    3

  (** Creating the main page, which contains an image table. *)
  let mainpage channel =
    table "border=\"0\" width=\"100%\"" gallery_width elements (fun element channel ->
      anchor element (
        hlink (page_link_of_element element) (
	  image (thumbnail element) (thumbnail_link_of_element element) element
        )
      ) channel;
      emptytag "br" "" channel;
      output_string channel (get_caption element)
    ) channel

  (** Creating a page dedicated to a single image. *)
  let elementpage element previous next channel =
    single_column_table "border=\"0\" width=\"100%\"" [

      (image (reduced element) (reduced_link_of_element element) element);

      (single_row_table "border=\"0\" width=\"100%\"" [

        (hlink (page_link_of_element previous) (print "Précédent"));

        (hlink ("index.html#" ^ element) (print title));

        (hlink (page_link_of_element next) (print "Suivant"));

      ]);

      (single_row_table "border=\"0\" width=\"100%\"" [

        (print (get_caption element));

        (print (mkdate (elementdate element)));

      ]);

    ] channel

  let gallery () =

    (* Generate an XHTML file per element. Link them together in a circular,
       doubly linked list. *)

    let a = Array.of_list elements in
    let n = Array.length a in
    let next i = a.(if i = n-1 then 0 else i+1)
    and prev i = a.(if i = 0 then n-1 else i-1) in
    Array.iteri (fun i element ->
      make (page element) (get_caption element) "img {border-width: 0}" (elementpage element (prev i) (next i));
      pulse()
    ) a;

    (* Generate an XHTML gallery file. *)

    let filename = gallerydir ^ "index.html" in

    let style = "img {border-width: 0; margin-top: 30px; margin-bottom: 10px}\n" ^
		(sprintf "td {width: %d%%}" (100 / gallery_width)) in

    make filename title style mainpage;

    gallerydir

end

