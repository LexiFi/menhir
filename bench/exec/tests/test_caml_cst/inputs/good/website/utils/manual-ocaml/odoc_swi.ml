(***********************************************************************)
(*                               MozCaml                               *)
(*                                                                     *)
(*                   Vincent Simonet & Maxence Guesdon                 *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: odoc_swi.ml,v 1.1 2004/05/05 16:23:55 simonet Exp $ *)

(** Generate a swish-e index from OCaml files. *)

open Odoc_info
open Swishe_index

open Module
open Type
open Value
open Exception
open Class

let field_name = "ident"
let field_complete_name = "longident"
let field_kind = "kind"
let field_type = "type"
let field_description = "description"
let field_abstract = "abstract"

let kind_module = "module"
let kind_module_type = "moduletype"
let kind_class = "class"
let kind_class_type = "classtype"
let kind_exception = "exception"
let kind_type = "type"
let kind_value = "value"
let kind_constructor = "constructor"
let kind_field = "field"
let kind_attribute = "attribute"
let kind_method = "method"

let a_opt f = function
    None -> None
  | Some v -> Some (f v)

let s_opt = function
    None -> ""
  | Some s -> s

let opt_abstract info_opt =
  match info_opt with
    None -> None
  | Some i ->
      match i.i_desc with
	None -> None
      |	Some text ->
	  let sent = Odoc_info.first_sentence_of_text text in
	  Some (string_of_text sent)

class generator =
  object(self)
    inherit Odoc_html.html as html

    method generate_value oc v =
      let content =
	Fields
	  [ field_complete_name, v.val_name ;
	    field_name, Name.simple v.val_name ;
	    field_kind, kind_value ;
	    field_type, Odoc_info.string_of_type_expr v.val_type ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info v.val_info) ;
	    field_abstract,
	      s_opt (opt_abstract v.val_info) ;
	  ] 
      in
      let url = Odoc_html.Naming.complete_value_target v in
      Swishe_index.output oc url content;

    method generate_type oc t =
      let content =
	Fields
	  [ field_complete_name, t.ty_name ;
	    field_name, Name.simple t.ty_name ;
	    field_kind, kind_type ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info t.ty_info) ;
	    field_abstract,
	      s_opt (opt_abstract t.ty_info) ;
	  ] 
      in
      let url = Odoc_html.Naming.complete_type_target t in
      Swishe_index.output oc url content

    method generate_exception oc e =
      let content =
	Fields
	  [ field_complete_name, e.ex_name ;
	    field_name, Name.simple e.ex_name ;
	    field_kind, kind_exception ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info e.ex_info) ;
	    field_abstract,
	      s_opt (opt_abstract e.ex_info) ;
	  ] 
      in
      let url = Odoc_html.Naming.complete_exception_target e in
      Swishe_index.output oc url content

    method generate_attribute oc a =
      let content =
	Fields
	  [ field_complete_name, a.att_value.val_name ;
	    field_name, Name.simple a.att_value.val_name ;
	    field_kind, kind_value ;
	    field_type, Odoc_info.string_of_type_expr a.att_value.val_type ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info a.att_value.val_info) ;
	    field_abstract,
	      s_opt (opt_abstract a.att_value.val_info) ;
	  ] 
      in
      let url = Odoc_html.Naming.complete_attribute_target a in
      Swishe_index.output oc url content;

    method generate_method oc m =
      let content =
	Fields
	  [ field_complete_name, m.met_value.val_name ;
	    field_name, Name.simple m.met_value.val_name ;
	    field_kind, kind_value ;
	    field_type, Odoc_info.string_of_type_expr m.met_value.val_type ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info m.met_value.val_info) ;
	    field_abstract,
	      s_opt (opt_abstract m.met_value.val_info) ;
	  ] 
      in
      let url = Odoc_html.Naming.complete_method_target m in
      Swishe_index.output oc url content;

    method generate_module_element oc e =
      match e with
	Element_module m -> self#generate_module oc m
      | Element_module_type mt -> self#generate_module_type oc mt
      | Element_class c -> self#generate_class oc c
      | Element_class_type ct -> self#generate_class_type oc ct
      | Element_value v -> self#generate_value oc v
      | Element_exception e -> self#generate_exception oc e
      | Element_type t -> self#generate_type oc t
      | Element_module_comment _ 
      | Element_included_module _ -> ()

    method generate_module_elements oc l =
      List.iter (self#generate_module_element oc) l

    method generate_class_element oc e =
      match e with
	Class_attribute v -> self#generate_attribute oc v
      | Class_method m -> self#generate_method oc m
      | Class_comment _ -> ()

    method generate_class_elements oc l =
      List.iter (self#generate_class_element oc) l

    method generate_class oc c =
      let content =
	Fields
	  [ field_complete_name, c.cl_name ;
	    field_name, Name.simple c.cl_name ;
	    field_kind, kind_class ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info c.cl_info) ;
	    field_abstract,
	      s_opt (opt_abstract c.cl_info) ;
	  ] 
      in
      let (file,_) = Odoc_html.Naming.html_files c.cl_name in
      Swishe_index.output oc file content;
      self#generate_class_elements oc (Class.class_elements c)

    method generate_class_type oc c =
      let content =
	Fields
	  [ field_complete_name, c.clt_name ;
	    field_name, Name.simple c.clt_name ;
	    field_kind, kind_class_type ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info c.clt_info) ;
	    field_abstract,
	      s_opt (opt_abstract c.clt_info) ;
	  ] 
      in
      let (file,_) = Odoc_html.Naming.html_files c.clt_name in
      Swishe_index.output oc file content;
      self#generate_class_elements oc (Class.class_type_elements c)

    method generate_module_type oc mt =
      let content =
	Fields
	  [ field_complete_name, mt.mt_name ;
	    field_name, Name.simple mt.mt_name ;
	    field_kind, kind_module_type ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info mt.mt_info) ;
	    field_abstract,
	      s_opt (opt_abstract mt.mt_info) ;
	  ] 
      in
      let (file,_) = Odoc_html.Naming.html_files mt.mt_name in
      Swishe_index.output oc file content;
      self#generate_module_elements oc (Module.module_type_elements mt)

    method generate_module oc m =
      let content =
	Fields
	  [ field_complete_name, m.m_name ;
	    field_name, Name.simple m.m_name ;
	    field_kind, kind_module ;
	    field_description, 
              s_opt (a_opt Odoc_info.string_of_info m.m_info) ;
	    field_abstract,
	      s_opt (opt_abstract m.m_info) ;
	  ] 
      in
      let (file,_) = Odoc_html.Naming.html_files m.m_name in
      Swishe_index.output oc file content;
      self#generate_module_elements oc (Module.module_elements m)

    method generate (modules : Module.t_module list) =
      let oc = open_out !Args.out_file in
      List.iter (self#generate_module oc) modules;
      close_out oc
  end


let _ = Args.set_doc_generator 
    (Some (new generator :> Args.doc_generator))



