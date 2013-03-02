(* $Header: /home/yquem/cristal/fpottier/cvs/picsou/syntax.ml,v 1.2 2006/01/08 10:59:14 fpottier Exp $ *)

(* The format of an abstract syntax tree, as created by the parser. *)

(* A database is a list of records. Records are a priori unordered. *)

type database = record list

(* A record contains a date, a category string, an auxiliary string, and a price. *)

and record = Record of date * string * string * float

(* Dates are stored both in seconds and as a record, i.e. in the form returned by Unix.mktime. *)

and date = float * Unix.tm

(* These exceptions are raised by the parser. *)

exception SyntaxError of string
exception MissingDefault

(* Converting date components to dates. *)

open Unix

let mkdate year mon mday =
  mktime {
    tm_sec = 0;
    tm_min = 0;
    tm_hour = 0;
    tm_mday = mday;
    tm_mon = mon - 1;
    tm_year = year - 1900;
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false
  }
