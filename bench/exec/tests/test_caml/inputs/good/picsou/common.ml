(* Functions common to [picsou] and [slave]. *)

open Syntax

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Loading a file. *)

let load filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  try

    Parser.database Lexer.token lexbuf

  with
  | Lexer.Error (error, start_loc, end_loc) ->
      Printf.printf "ERROR: %s: %s at characters %d-%d.\n%!"
	filename
	(Lexer.report_error error)
        start_loc
        end_loc;
      exit 1
  | SyntaxError message ->
      Printf.printf "ERROR: %s: %s.\n%!"
	filename
	message;
      exit 1
  | MissingDefault ->
      Printf.printf "ERROR: %s: cannot leave blank fields in the first record.\n%!"
	filename;
      exit 1

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Filters. *)

let date_of (Record(date, _, _, _)) =
  date

let category (Record(_, category, _, _)) =
  category

let auxiliary (Record(_, _, auxiliary, _)) =
  auxiliary

let price (Record(_, _, _, price)) =
  price

let constant_date year month day =
  Unix.mktime {
  Unix.tm_sec = 0;
  Unix.tm_min = 0;
  Unix.tm_hour = 0;
  Unix.tm_mday = day;
  Unix.tm_mon = month - 1;
  Unix.tm_year = year - 1900;
  Unix.tm_wday = 0;
  Unix.tm_yday = 0;
  Unix.tm_isdst = false
}

let leq_dates (float1, _) (float2, _) =
  float1 <= float2

let more_recent date record =
  leq_dates date (date_of record)

let less_recent date record =
  leq_dates (date_of record) date

let has_category cat record =
  cat = category record

let has_subcategory cat record =
  cat = auxiliary record

let has_not_category cat record =
  cat <> category record

let has_auxiliary aux record =
  aux = auxiliary record

let rec combine plist record =
  match plist with
    [] ->
      true
  | predicate :: plist ->
      (predicate record) & (combine plist record)

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Displaying figures. *)

let display_euros =
  ref true

let display show_currency price =
  let price =
    if !display_euros then
      price
    else
      Currency.euros_to_francs price in
  if show_currency then
    if !display_euros then
      Printf.sprintf "%.2f euros" price
    else
      Printf.sprintf "%.2f francs" price
  else
    Printf.sprintf "%.2f" price

