(***********************************************************************)
(*                               Hump                                  *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Generating [.rdf] files for MozCaml. *)

open Rdf

let topic_url t =
  Printf.sprintf "%s?topic=%d"
    Config.raw_loc_cgi_hump
    t.Db.Props.id
													    
let rec item_topic t =
  let atts = 
   [
     "HUMP:label", t.Db.Props.name ;
     "HUMP:ref", topic_url t ;
   ]
  in
  match List.map item_topic (Props.children t) with
    [] -> Desc atts
  | l -> Seq (atts, l)
  
let generate oc =
  let fmt = Format.formatter_of_out_channel oc in
  let topics = 
    let root = Props.kind_root Types.Topic in
    List.map item_topic (Props.children root)
  in
  let t = 
    { namespaces = [ "HUMP", "http://mozcaml.inria.fr/hump#"] ;
      content = [Seq (["about","urn:mozcaml:topics"], topics)] ;
    } 
  in
  Rdf.output fmt t ;
  Format.pp_print_flush fmt ()
