(* $Header: /home/yquem/cristal/fpottier/cvs/photos/search.ml,v 1.8 2005/12/11 21:36:47 fpottier Exp $ *)

open Printf
open Util
open Database

exception SyntaxError of int * string

type query =
  | Name of (string -> bool)
  | DateGreaterThan of float
  | DateLessThan of float
  | Caption of (string -> bool)
  | Property of string
  | Not of query
  | And of query * query
  | Or of query * query
  | True

let rec evaluate query element =
  match query with
  | Name p ->
      p element
  | DateGreaterThan date ->
      elementdate element >= date
  | DateLessThan date ->
      elementdate element <= date
  | Caption p ->
      p (get_caption element)
  | Property property ->
      get_property element property
  | Not query ->
      not (evaluate query element)
  | And (query1, query2) ->
      (evaluate query1 element) && (evaluate query2 element)
  | Or (query1, query2) ->
      (evaluate query1 element) || (evaluate query2 element)
  | True ->
      true

(* To optimize queries, look for date criteria first, and exploit the
   cache, which allows efficient extraction of date ranges. *)

let rec preprocess ((map, residual_query) as accu) = function
  | [] ->
      accu
  | (DateGreaterThan date) :: queries ->
      preprocess (Cache.DateMap.above date map, residual_query) queries
  | (DateLessThan date) :: queries ->
      preprocess (Cache.DateMap.below date map, residual_query) queries
  | (And (query1, query2)) :: queries ->
      preprocess accu (query1 :: query2 :: queries)
  | query :: queries ->
      preprocess (map, And (query, residual_query)) queries

let search query =
  let map, query = preprocess (!Cache.map, True) [ query ] in
  List.rev (
    Cache.DateMap.fold (fun _ elements accu ->
      List.fold_left (fun accu element ->
	if evaluate query element then
	  element :: accu
	else
	  accu
      ) accu elements
    ) map []
  )

