(***********************************************************************)
(*                               MozCaml                               *)
(*                                                                     *)
(*                   Vincent Simonet & Maxence Guesdon                 *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Outputting RDF files. *)

open Format

type t =
    { namespaces: (string * string) list;
      content: item list
    }

and item =
    Seq of attributes * item list
  | Desc of attributes

and attributes =
    (string * string) list



(***************************************************************************)
(** {2 Pretty-print} *)

let remove_evil_chars s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '<' -> Buffer.add_string buf "&lt;"
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let output_string ppf string =
  fprintf ppf "%s" (remove_evil_chars string)

let rec output_attributes ppf = function
    [] -> ()
  | [name, v] -> 
      fprintf ppf "%s=\"%a\"" name output_string v
  | (name, v) :: tl ->
      fprintf ppf "%s=\"%a\"@ %a" 
	name output_string v output_attributes tl

let rec output_item_list ppf list =
  ignore (List.fold_left (fun first item ->
    if first then fprintf ppf "@ " ;
    fprintf ppf
      "<RDF:li>@ @[<v 2>%a@]@ </RDF:li>" 
      output_item item;
    false
  ) true list)

and output_item ppf = function
    Seq (attr, items) ->
      fprintf ppf
	"<RDF:Seq @[<v>%a@]>@ @[<v 2>%a@]@ </RDF:Seq>"
	output_attributes attr output_item_list items
  | Desc attr ->
      fprintf ppf 
	"<RDF:Description @[<v>%a@]/>" output_attributes attr



let output ppf t =      

  let namespaces ppf =
    output_attributes ppf
      (List.map (function name, url -> "xmlns:" ^ name, url) t.namespaces)
  in
  let content ppf =
    ignore (List.fold_left (fun first item ->
      if first then fprintf ppf "@ " ;
      fprintf ppf "%a" output_item item;
      false
    ) true t.content)
  in

  fprintf ppf
    "@[<v><?xml version=\"1.0\"?>@ \
    <RDF:RDF @[<v>xmlns:RDF=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"@ \
		  %t@]>@ @[<v 2>%t@]@ </RDF:RDF>@]"
  namespaces content
  

