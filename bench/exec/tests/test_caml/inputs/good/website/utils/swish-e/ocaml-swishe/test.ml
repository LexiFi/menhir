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

(* $Id: test.ml,v 1.5 2005/03/14 15:26:19 guesdon Exp $ *)



open Swishe_excerpt


let fprintf_swishe_value oc = function
    Swishe_api.String stg -> output_string oc stg
  | Swishe_api.Int i -> output_string oc (string_of_int i)
  | Swishe_api.Bool b -> output_string oc (if b then "true" else "false")
  | Swishe_api.StringList list ->
      ignore (List.fold_left (fun notfirst stg ->
        if notfirst then output_string oc ", ";
        output_string oc stg;
        true
     ) false list)
  | Swishe_api.Unknown -> output_string oc "[unknown]"

let () =

for i = 1 to 1 do
  let query = List.tl (Array.to_list Sys.argv) in
  let handle = Swishe_api.init ["/net/brion/infosystems/www/camltest/swish-e/index/hump.index"] in

  let answer = Swishe_api.query handle (String.concat " " query) in

  List.iter (function result ->
    Printf.printf "[%4i] %s\n       %s\n"
      (string_of_int (Swishe_api.property result "swishrank"))
      (Swishe_api.property result "swishdocpath")
      (Swishe_api.property result "swishtitle");
(*    Swishe_excerpt.output_excerpt 256 query stdout
      (Swishe_api.property result "test");
      *)
   print_newline();
   List.iter (function w ->
   fprintf_swishe_value stdout (Swishe_api.StringList (Swishe_api.fuzzy_word result w));
   print_newline();
   ) query;
   print_string "\n\n"
      
  ) answer.Swishe_api.results;

  Printf.printf "%i / %i" (List.length answer.Swishe_api.results)
    (answer.Swishe_api.hits);
  print_newline ();

  print_newline ()

done;

  flush stdout;

