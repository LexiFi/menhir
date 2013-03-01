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

(** RDF generator from OCaml files. *)

open Odoc_info
open Rdf

open Module
open Type
open Value

type group_element = 
    Grp_module of Name.t (* complete name of the module *)
  | Grp_sub of string (* file describing the the sub group *)

type group = {
    grp_text : text ;
    grp_eles : group_element list ;
  } 

let group_files = ref []

let analyse_group_file f =
  let s = Odoc_misc.input_file_as_string f in
  let (_, info_opt) = 
    Odoc_comments.Basic_info_retriever.first_special f ("(** "^s^" *)")
  in
  match info_opt with
    None -> { grp_text = [Raw ""] ; grp_eles = [] ; }
  | Some i ->
      let rec eles = function
	  [] -> []
	| ("module", t) :: q ->
	    (Grp_module (string_of_text t)) :: (eles q)
	| ("sub", t) :: q ->
	    (Grp_sub (string_of_text t)) :: (eles q)
	| _ :: q -> eles q
      in
      let t = 
	match i.i_desc with
	  None -> [Raw ""]
	| Some t -> t
      in
      { grp_text = t ;
	grp_eles = eles i.i_custom ;
      }	

let analyse_group_files l =
  List.map analyse_group_file l


class generator =
  object(self)
    inherit Odoc_html.html as html

    method analyse_elements cur_mod_name cur_level l =
      match l with
      |	[] -> [], []
      | (Element_module m) :: q ->
	  let item = self#item_module m in
	  let (l, remain) =
	    self#analyse_elements cur_mod_name cur_level q
	  in
	  ((item :: l), remain)
      | (Element_module_type mt) :: q ->
	  let item = self#item_module_type mt in
	  let (l, remain) =
	    self#analyse_elements cur_mod_name cur_level q
	  in
	  ((item :: l), remain)
      | (Element_module_comment t) :: q ->
	  (
	   match Odoc_info.get_titles_in_text t with
	     (n,label_opt,title) :: _ -> 
	       let label = html#create_title_label (n,label_opt,title) in
	       let (file,_) = Odoc_html.Naming.html_files cur_mod_name in
	       let url = Odoc_html.Naming.complete_label_target 
		   (Name.concat cur_mod_name label)
	       in
	       let s = Odoc_info.string_of_text title in
	       let atts =
		 [ "LIBRARY:label", s ;
		   "LIBRARY:ref", url ;
		 ]
	       in
	       if n > cur_level then
		 let (subs, remain) = 
		   self#analyse_elements cur_mod_name n q
		 in
		 let (l, l2) = 
		   self#analyse_elements cur_mod_name cur_level remain
		 in
		 ((Seq (atts, subs) :: l), l2)
	       else
		 ([], (Element_module_comment t) :: q)		 
	   | _ ->
	       self#analyse_elements cur_mod_name cur_level q
	  )
      |	_ :: q ->
	  self#analyse_elements cur_mod_name cur_level q

    method item_module m =
      let atts = 
	[ "LIBRARY:label", Name.simple m.m_name ;
	  "LIBRARY:ref", fst (Odoc_html.Naming.html_files m.m_name) ;
	]
      in
      let (items,_) = self#analyse_elements m.m_name 0 
	  (Module.module_elements m)
      in
      match items with
	[] -> Desc atts
      |	_ -> Seq (atts, items)

    method item_module_type mt =
      let atts = 
	[ "LIBRARY:label", Name.simple mt.mt_name ;
	  "LIBRARY:ref", fst (Odoc_html.Naming.html_files mt.mt_name) ;
	]
      in
      let (items,_) = self#analyse_elements mt.mt_name 0 
	  (Module.module_type_elements mt) 
      in
      match items with
	[] -> Desc atts
      |	_ -> Seq (atts, items)

    method item_group_ele all_modules ele =
      match ele with
	Grp_module name ->
	  (
	   try 
	     let m = List.find (fun m -> m.m_name = name) all_modules in
	     self#item_module m
	   with Not_found -> 
	     let mes = "Module "^name^" not found." in
	     warning mes;
	     Desc ["LIBRARY:label", mes ;
		   "LIBRARY:ref", "" ;
		  ]
	  )
      |	Grp_sub file ->
	  let group = analyse_group_file file in
	  self#item_group all_modules group

    method item_group all_modules grp =
      let atts = 
	[ "LIBRARY:label", 
	  string_of_text (first_sentence_of_text grp.grp_text) ;
	  "LIBRARY:ref", "" ;
	]
      in
      let items = List.map (self#item_group_ele all_modules) grp.grp_eles in
      match items with
	[] -> Desc atts
      |	_ -> Seq (atts, items)

    method generate_tree modules =
      let groups = analyse_group_files !group_files in
      let items =
	match groups with
	  [] -> List.map self#item_module modules
	| l -> 
	    let all_modules = Search.modules modules in
	    List.map (self#item_group all_modules) l
      in
      let t =
	{ namespaces = [ "LIBRARY", "http://caml.inria.fr/library-ocaml#"] ;
	  content = [Seq (["about","urn:caml:library-ocaml"], items)] ;
	} 
      in
      let oc = open_out !Args.out_file in
      let fmt = Format.formatter_of_out_channel oc in
      Rdf.output fmt t;
      Format.pp_print_flush fmt ();
      close_out oc

    method index_file suffix =
      let s = 
	try Filename.chop_extension !Args.out_file 
	with _ -> !Args.out_file
      in
      s^"_"^suffix^".rdf"

    method generate_index_types modules =
      let l = Search.types modules in
      let l_sorted = List.sort
	  (fun t1 t2 -> compare (Name.simple t1.ty_name) (Name.simple t2.ty_name))
	  l
      in
      let item t = 
	Desc
	  [
	    "INDEX:name", Name.simple t.ty_name ;
	    "INDEX:module", Name.father t.ty_name ;
	    "INDEX:ref", Odoc_html.Naming.complete_type_target t ;
	  ]	
      in
      let items = List.map item l_sorted in
      self#generate_libindex "types" items

    method generate_index_values modules =
      let l = Search.values modules in
      let l_sorted = List.sort
	  (fun v1 v2 -> compare (Name.simple v1.val_name) (Name.simple v2.val_name))
	  l
      in
      let item v = 
	Desc
	  [
	    "INDEX:name", Name.simple v.val_name ;
	    "INDEX:module", Name.father v.val_name ;
	    "INDEX:ref", Odoc_html.Naming.complete_value_target v ;
	  ]	
      in
      let items = List.map item l_sorted in
      self#generate_libindex "values" items

    method generate_index_modules modules =
      let l = Search.modules modules in
      let l_sorted = List.sort
	  (fun m1 m2 -> compare (Name.simple m1.m_name) (Name.simple m2.m_name))
	  l
      in
      let item m = 
	Desc
	  [
	    "INDEX:name", Name.simple m.m_name ;
	    "INDEX:module", Name.father m.m_name ;
	    "INDEX:ref", fst (Odoc_html.Naming.html_files m.m_name) ;
	  ]	
      in
      let items = List.map item l_sorted in
      self#generate_libindex "modules" items

    method generate_index_module_types modules =
      let l = Search.module_types modules in
      let l_sorted = List.sort
	  (fun m1 m2 -> compare (Name.simple m1.mt_name) (Name.simple m2.mt_name))
	  l
      in
      let item m = 
	Desc
	  [
	    "INDEX:name", Name.simple m.mt_name ;
	    "INDEX:module", Name.father m.mt_name ;
	    "INDEX:ref", fst (Odoc_html.Naming.html_files m.mt_name) ;
	  ]	
      in
      let items = List.map item l_sorted in
      self#generate_libindex "module_types" items

    method generate_libindex kind items =
      let t = 
	{ namespaces = [ "INDEX", "http://mozcaml.inria.fr/libindex#"] ;
	  content = [Seq (["about","urn:mozcaml:"^kind], items)] ;
	} 
      in
      let oc = open_out (self#index_file kind) in
      let fmt = Format.formatter_of_out_channel oc in
      Rdf.output fmt t;
      Format.pp_print_flush fmt ();
      close_out oc

    method generate (modules : Module.t_module list) =
      self#generate_tree modules;
      self#generate_index_types modules ;
      self#generate_index_values modules ;
      self#generate_index_modules modules ;
      self#generate_index_module_types modules
  end


let _ = Args.set_doc_generator 
    (Some (new generator :> Args.doc_generator))

let _ = Args.add_option 
    ("-grp", Arg.String (fun s -> group_files := !group_files @ [s]),
     "<file>  indicate a file describing a group of modules")


