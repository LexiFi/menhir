(* $Header: /home/yquem/cristal/fpottier/cvs/picsou/picsou.ml,v 1.10 2006/02/08 21:14:12 fpottier Exp $ *)

open Syntax
open Common

(* ----------------------------------------------------------------------------------------------------------------- *)
(* The global database. *)

let database =
  ref ([] : database)

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Reporting errors. *)

let failure info message =
  Printf.printf "%s: %s.\n" info message;
  flush stdout;
  exit(1)

(* ----------------------------------------------------------------------------------------------------------------- *)
(* The program's entry point. *)

(* [load filename] loads the file named [filename]. The file's contents are parsed and added to the global database. *)

let load filename =
  database := !database @ load filename

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Converting a database back to text. *)

let to_text database =
  let buffer = Buffer.create (40 * (List.length database)) in
  List.iter (function Record((_, tm), cat, aux, price) ->
    Printf.bprintf buffer "%04d/%02d/%02d\t%s\t(%s)\t%s\n"
      (1900 + tm.Unix.tm_year)
      (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday
      cat
      aux
      (display false price)
  ) database;
  Buffer.contents buffer

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Printing the contents of a database. *)

let total database =
  List.fold_left (fun total record ->
    (price record) +. total
  ) 0. database

let print database =
  print_string (to_text database);
  Printf.printf "Total expense is %s.\n" (display true (total database));
  flush stdout

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Determining the categories in a database. *)

let rec uniq list =
  match list with
  | []
  | [ _ ] ->
      list
  | x :: y :: rest ->
      if x = y then uniq (y :: rest)
      else x :: uniq (y :: rest)

let categories database =
  uniq (Sort.list (<=) (List.map category database))

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Printing a database per category. *)

let totals database =
  List.iter (fun cat ->
    Printf.printf "%s: %s.\n" cat (display true (total (List.filter (has_category cat) database)))
  ) (categories database);
  Printf.printf "Total: %s.\n" (display true (total database));
  flush stdout


(* ----------------------------------------------------------------------------------------------------------------- *)
(* Main program. *)

let parse_date info string =
  let lexbuf = Lexing.from_string string in
  try
    Parser.date Lexer.token lexbuf
  with
  | Lexer.Error (error, start_loc, end_loc) ->
      failure info (Printf.sprintf
	"%s at characters %d-%d"
	(Lexer.report_error error)
        start_loc
        end_loc)
  | SyntaxError message ->
      failure info message

let criteria =
  ref []
let one_category =
  ref false

let set_from_date string =
  criteria := (more_recent (parse_date "-from" string)) :: !criteria
let set_to_date string =
  criteria := (less_recent (parse_date "-to" string)) :: !criteria

let last_day year month =
  match month with
  | 1
  | 3
  | 5
  | 7
  | 8
  | 10
  | 12 ->
      31
  | 4
  | 6
  | 9
  | 11 ->
      30
  | 2 ->
      if year mod 4 = 0 then
	29
      else
	28
  | _ ->
      failure "-m" (Printf.sprintf
		      "invalid month: %2d"
		      month)

let set_month month =
  let month = try
    int_of_string month
  with Failure _ ->
    failure "-m" (Printf.sprintf
		      "invalid month: %s"
		      month) in
  let now = Unix.gmtime (Unix.time()) in
  let this_year = 1900 + now.Unix.tm_year in
  let this_month = now.Unix.tm_mon + 1 in
  let year = if month <= this_month then this_year else this_year - 1 in
  criteria :=
    (more_recent (constant_date year month 1)) ::
    (less_recent (constant_date year month (last_day year month))) ::
    !criteria

let set_category cat =
  one_category := true;
  criteria := (has_category cat) :: !criteria

let set_subcategory cat =
  one_category := true;
  criteria := (has_subcategory cat) :: !criteria

let _ =

  (* Parse the command line. *)

  Arg.parse [
    "-from", Arg.String set_from_date, "<YYYY/MM/DD> discards entries older than given date";
    "-to", Arg.String set_to_date, "<YYYY/MM/DD> discards entries newer than given date";
    "-m", Arg.String set_month, "<MM> discards entries not in given month (of current year)";
    "-cat", Arg.String set_category, "<category> discards entries not carrying this category";
    "-subcat", Arg.String set_subcategory, "<sub-category> discards entries not carrying this sub-category";
    "-euros", Arg.Set display_euros, "displays figures in Euros"
  ] load "Picsou, the account helper.";

  (* If no files were explicitly loaded, open the default one. *)

  if !database = [] then
    load "comptes";

  (* Apply the specified filters. *)

  database := List.filter (combine !criteria) !database;

  (* If the query was restricted to one category, print it explicitly. Otherwise, only print totals
     per category. (This behavior is usually what is desired.) *)

  (if !one_category then print else totals) !database

