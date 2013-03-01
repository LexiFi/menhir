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

(* $Id: swishe_api.ml,v 1.4 2005/02/17 14:09:39 guesdon Exp $ *)

open Printf

(*************************************************************************
Errors
*************************************************************************)

type error =
    Index_file_not_found
  | Unknown_index_file_format
  | No_words_in_search
  | Words_too_common
  | Index_file_is_empty
  | Index_file_error
  | Unknown_property_name_in_search_display
  | Unknown_property_name_in_search_sort
  | Invalid_property_type
  | Unknown_metaname
  | Unique_wildcard_not_allowed_in_word
  | Word_not_found
  | Swish_listresults_eof
  | Header_read_error
  | Invalid_swish_handle
  | Invalid_results_handle
  | Search_word_too_big
  | Query_syntax_error
  | Prop_limit_error
  | Wildcard_not_allowed_within_word
  | Unknown_error of int


exception Error of error * string

let error code msg =
  raise (Error (code, msg))

let _ = 
  Callback.register "Swishe.error" error

let string_of_error = function
  | No_words_in_search -> "No search words specified"
  | Words_too_common -> "All search words too common to be useful"
  | Unknown_property_name_in_search_display ->
      "Unknown property name in display properties"
  | Unknown_property_name_in_search_sort ->
      "Unknown property name to sort by"
  | Invalid_property_type -> "Invalid property type"
  | Unknown_metaname -> "Unknown metaname"
  | Unique_wildcard_not_allowed_in_word ->
      "Single wildcard not allowed as word"
  | Wildcard_not_allowed_within_word -> "Wildcard not allowed within a word"
  | Word_not_found -> "Word not found"
  | Search_word_too_big -> "Search word exceeded maxwordlimit setting"
  | Query_syntax_error ->
      "Syntax error in query (missing end quote or unbalanced parenthesis?)"
  | Prop_limit_error -> "Failed to setup limit by property"
  | Swish_listresults_eof -> "No more results"
  | Header_read_error -> "Index Header Error"
  | Index_file_not_found -> "Could not open index file"
  | Unknown_index_file_format -> "Unknown index file format"
  | Index_file_is_empty -> "Index file(s) is empty"
  | Index_file_error -> "Index file error"
  | Invalid_swish_handle -> "Invalid swish handle"
  | Invalid_results_handle -> "Invalid results object"
  | Unknown_error i -> Printf.sprintf "Unknown Swish-e error (code: %i)" i



type swish_value =
    String of string
  | Int of int
  | Bool of bool
  | StringList of string list
  | Unknown



(*************************************************************************
Handles
*************************************************************************)

type handle

external unsafe_init: string -> handle = "camlswishe_init"

(* TEMPORARY
   Que fait-on si un nom de fichier comporte le caractère double-quote
*)
let init files =
  unsafe_init ("\"" ^ (String.concat "\" \"" files) ^ "\"")

external headers: handle -> (string * string * swish_value) list
    = "camlswishe_headers"



(*************************************************************************
Queries
*************************************************************************)

type result
external property: result -> string -> string = "camlswishe_property"
external property_int: result -> string -> int = "camlswishe_property_int"
external fuzzy_word: result -> string -> string list = "camlswishe_fuzzy_word"

type sort_order =
    Ascending of string
  | Descending of string

let string_of_sort_order = function
    Ascending property -> sprintf "%s asc" property
  | Descending property -> sprintf "%s desc" property



type answer = {
    hits: int;
    results: result list;
    parsed_words: string list;
    removed_stopwords: string list;
  }

external unsafe_query: handle -> string -> string -> answer
    = "camlswishe_query"

let query handle ?(sort=[]) query =
  let sort_string =
    String.concat " " (List.map (function
        Ascending property -> sprintf "%s asc" property
      | Descending property -> sprintf "%s desc" property
      ) sort)
  in
  (unsafe_query handle sort_string query)

