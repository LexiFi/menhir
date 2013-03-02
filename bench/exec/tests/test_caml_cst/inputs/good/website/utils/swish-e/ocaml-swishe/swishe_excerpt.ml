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

(* $Id: swishe_excerpt.ml,v 1.3 2005/02/17 14:09:40 guesdon Exp $ *)



(*************************************************************************)
(** Configuration *)

let wordchars = Array.make 256 false
let beginchars = Array.make 256 false
let endchars = Array.make 256 false
let ignorefirstchars = Array.make 256 false
let ignorelastchars = Array.make 256 false

let blankchars = Array.make 256 false

let fill_table tbl stg =
  for i = 0 to String.length stg - 1 do
    tbl.(int_of_char stg.[i]) <- true;
  done

(* Configuration par défaut *)
let _ = 
  let isolatin1 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmno\
      pqrstuvwxyzªµºÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñò\
      óôõöøùúûüýþÿ" 
  in
  fill_table wordchars isolatin1;
  fill_table beginchars isolatin1;
  fill_table endchars isolatin1;

  fill_table blankchars " \009\010\012\013"



(*************************************************************************)
(** Recherche des occurences *)

let find_occurences filter stg =
  let len = String.length stg in
  let words = ref [] in
  let rec lex_out pos =
    if pos < len then begin
      let code = int_of_char stg.[pos] in
      if wordchars.(code) then lex_word_begin pos
      else lex_out (pos + 1)
    end
  and lex_word_begin pos =
    if pos < len then begin
      let code = int_of_char stg.[pos] in
      if ignorefirstchars.(code) then lex_word_begin (pos + 1)
      else if beginchars.(code) then 
        lex_word pos (if endchars.(code) then pos else pos - 1) (pos + 1)
      else lex_skip_word (pos + 1)
    end 
  and lex_word start_pos last_pos pos =
    if pos < len && wordchars.(int_of_char stg.[pos]) then begin
      lex_word start_pos (if endchars.(int_of_char stg.[pos])
      then pos else last_pos) (pos + 1)
    end else begin
      let len' = last_pos - start_pos + 1 in
      if start_pos <= last_pos && filter (String.sub stg start_pos len')
      then words := (start_pos, len') :: !words;
      lex_out (pos + 1)
    end
  and lex_skip_word pos =
    if pos < len then begin
      if wordchars.(int_of_char stg.[pos]) then lex_skip_word (pos + 1)
      else lex_out (pos + 1)
    end
  in
  lex_out 0;
  List.rev !words



(*************************************************************************)
(** Excerpt *)

let excerpt length stg occurences =

  let stg_len = String.length stg in

  let rec find_occ best_pos best_length best_count count l1 l2 =
    match l1, l2 with
    | _, [] ->
        (best_pos, best_length)
    | [], _ :: _ -> assert false
    | (pos1, _) :: tl1, (pos2, len2) :: tl2 ->
        if pos2 + len2 - pos1 > length then
          find_occ best_pos best_length best_count (count - 1) tl1 l2
        else begin
          if count > best_count 
          then find_occ pos1 (pos2 + len2 - pos1) count (count + 1)  l1 tl2
          else find_occ best_pos best_length best_count (count + 1)  l1 tl2
        end

  in

  let pos0, length0 = find_occ 0 0 0 1 occurences occurences in

  let rec find_start pos =
    if pos < pos0 && not blankchars.(int_of_char stg.[pos]) 
    then find_start (pos + 1)
    else strip pos
  and strip pos =
    if pos < stg_len - 1 && blankchars.(int_of_char stg.[pos]) 
    then strip (pos + 1)
    else pos
  in

  let pos_start = find_start (max 0 (pos0 - ((length - length0) / 2))) in

  let rec find_stop pos' =
    if pos' > pos0 + length0 && not blankchars.(int_of_char stg.[pos' - 1])
    then find_stop (pos' - 1)
    else strip pos'
  and strip pos' =
    if pos' > pos0 + length0 && blankchars.(int_of_char stg.[pos' - 1])
    then strip (pos' - 1)
    else pos' 
  in

  let pos_stop = find_stop (min stg_len (pos_start + length)) in

  pos_start, pos_stop




(*************************************************************************)
(** *)

let query_table : (string, unit) Hashtbl.t = Hashtbl.create 7

let default_filter word =
  Hashtbl.mem query_table (String.lowercase word)



let map_excerpt length query normal highlighted stg =

  Hashtbl.clear query_table;

  List.iter (function word ->
    Hashtbl.add query_table (String.lowercase word) ()
  ) query;

  let occ = find_occurences default_filter stg in
  let pos_start, pos_stop = excerpt length stg occ in

  let rec map pos = function
      [] -> 
        assert (pos_stop >= pos);
        normal (String.sub stg pos (pos_stop - pos))
    | (o_pos, o_len) :: rem ->
        if o_pos < pos then map pos rem else begin
          if o_pos < pos_stop then begin
            normal (String.sub stg pos (o_pos - pos));
            if o_pos + o_len > pos_stop then
              highlighted (String.sub stg o_pos (pos_stop - pos))
            else begin
              highlighted (String.sub stg o_pos o_len);
              map (o_pos + o_len) rem
            end
          end
          else normal (String.sub stg pos (pos_stop - pos))
        end
  in
  map pos_start occ
  


let output_excerpt length query oc stg =
  map_excerpt length query
    (output_string oc)
    (fun s -> Printf.fprintf oc "[%s]" s)
    stg

  
  

