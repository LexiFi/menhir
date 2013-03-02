(**************************************************************************)
(*                                                                        *)
(*             Objective Caml interface to the Swish-e library            *)
(*                                                                        *)
(*           Vincent Simonet, projet Cristal, INRIA Rocquencourt          *)
(*                                                                        *)
(*  Copyright 2003 2004                                                   *)
(*  Institut National de Recherche en Informatique et en Automatique      *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Library General Public License (see file LICENSE).            *)
(*                                                                        *)
(*  Vincent.Simonet@inria.fr        http://cristal.inria.fr/~simonet/     *)
(*                                                                        *)
(**************************************************************************)

open Printf



(**********************************************************************)
(** {2 Representation of files} *)

(** Values of type [content] describes the content of a file.  Several 
    formats are available.
 *)
type content =

    Html of string
	(** A plain HTML file.  The string must contain the content of the 
	    file.
	 *)

  | Xml of string
	(** A plain XML file.  The string must contain the content of the
	    file.
	 *)

  | Text of string
	(** A plain text file.  The string must contain the content of the
	    file.
	 *)

  | Fields of (string * string) list
	(** A list of fields.  Each element of the list is a pair formed of
	    the name of the field and the content of the field.  Fields name
	    can only characters which are valids in XML tags.
	    Swish-e recognize two particular fields name: swishtitle (the
	    title of the file) and swishdescription (the description of the
	    document).
	 *)



(** Encoding of fields content.
 *)
let encoding = ref "ISO-8859-1"




(** Information about a file are given by a record of type [file_info].
 *)
type file_info =

    { path_name: string;
        (** The name of the file you are indexing.  This can be any string,
	    so for example it could be an ID of a record in a darabase, a 
	    URL or a simple file name.  This field is required and cannot
	    contain '\n'.
	 *)

      last_mtime: Int64.t option;
        (** The last modification time of the file, and must be a time 
	    stamp (seconds since the Epoch on your platform).  This is
	    optional.
	 *)

      content: content;
        (** The content of the file.
	 *)

    } 





(**********************************************************************)
(** {2 Translation of list of fields into XML} *)

let add_string_escape buf string =

  let len = String.length string in

  let rec aux origin pos =

    if pos = len then Buffer.add_substring buf string origin (pos - origin)
    else 
      match string.[pos] with
	'<' ->
	  Buffer.add_substring buf string origin (pos - origin);
	  Buffer.add_string buf "&lt;";
	  aux (pos + 1) (pos + 1)
      |	'>' ->
	  Buffer.add_substring buf string origin (pos - origin);
	  Buffer.add_string buf "&gt;";
	  aux (pos + 1) (pos + 1)
      |	'&' ->
	  Buffer.add_substring buf string origin (pos - origin);
	  Buffer.add_string buf "&amp;";
	  aux (pos + 1) (pos + 1)
      |	_ -> aux origin (pos + 1)

  in

  aux 0 0



let xml_of_fields list =

  let buf = Buffer.create 128 in

  Buffer.add_string buf 
    (sprintf "<?xml version=\"1.0\" encoding=\"%s\"?>\n<swishdefault>\n" 
       !encoding);

  List.iter (function fieldname, fieldcontent ->
    Buffer.add_string buf (sprintf "<%s>" fieldname);
    add_string_escape buf fieldcontent;
    Buffer.add_string buf (sprintf "</%s>\n" fieldname)

  ) list;

  Buffer.add_string buf "</swishdefault>\n";

  Buffer.contents buf



(**********************************************************************)
(** {2 Output} *)

let expand_content = function

    Html s | Xml s | Text s -> s

  | Fields list -> xml_of_fields list



let document_type = function
    Html _ -> "HTML*"
  | Xml _ | Fields _ -> "XML*"
  | Text _ -> "TXT*"


let remove_evil_chars s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' ->  ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf


let output oc path_name ?mtime content =
  
  let plain_content = expand_content content in

  fprintf oc "Path-Name: %s\n" (remove_evil_chars path_name);
  fprintf oc "Content-Length: %i\n" (String.length plain_content);
  begin match mtime with
    None -> ()
  | Some time -> 
      fprintf oc "Last-Mtime: %Ld\n" time
  end;
  fprintf oc "Document-Type: %s\n" (document_type content);
  fprintf oc "\n%s" plain_content

  

let output_info oc info =
  output oc info.path_name ?mtime:info.last_mtime info.content
