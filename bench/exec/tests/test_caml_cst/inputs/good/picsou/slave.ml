open Printf
open Syntax
open Common
open Instruction

(* Organize records by date. *)

module Date = struct
  type t = date
  let compare : t -> t -> int = fun (f1, _) (f2, _) ->
    compare f1 f2
end

module DateMap = struct
  include MyMap.Make(Date)
  let update key data m =
    let previous =
      try
	find key m
      with Not_found ->
	[]
    in
    add key (data :: previous) m
end

(* Load the file. *)

let filename =
  "/Users/magali/picsou/comptes" (* TEMPORARY avoid hard coding *)

let dateMap =
  List.fold_left (fun dateMap (Record (date, _, _, _) as record) ->
    DateMap.update date record dateMap
  ) DateMap.empty (load filename)

(* Isolate a portion of the data, based on a date range. This portion
   changes whenever a new date range is selected. *)

let portion =
  ref dateMap

(* Out of this portion, isolate a sub-portion by specifying a regexp. *)

let regexp =
  ref (Str.regexp "")

let subportion =
  ref !portion

let matches s =
  try
    let (_ : int) = Str.search_forward !regexp s 0 in
    true
  with Not_found ->
    false

let filter () =
  subportion :=
    DateMap.fold (fun date records accu ->
      match
	List.filter (function Record (_, cat, subcat, _) ->
	  (matches cat) || (matches subcat)
	) records
      with
      | [] ->
	  accu
      | records ->
	  DateMap.add date records accu
    ) !portion DateMap.empty

(* Respond to selection commands. *)

let selectPortion date1 date2 =
  portion := DateMap.split (Some date1, Some date2) dateMap;
  filter()

let selectKey key =
  regexp := Str.regexp_string_case_fold key; (* TEMPORARY regexp_string? case_fold? *)
  filter()

(* Spit out the current data. *)

let buffer =
  Buffer.create 65536

let spit () =
  let total =
    DateMap.fold (fun _ records total ->
      List.fold_left (fun total (Record((_, tm), cat, aux, price)) ->
	Printf.bprintf buffer "%04d/%02d/%02d\t%s\t%s\t%s\n"
	  (1900 + tm.Unix.tm_year)
	  (tm.Unix.tm_mon + 1)
	  tm.Unix.tm_mday
	  cat
	  aux
	  (display false price);
	total +. price
      ) total records
    ) !subportion 0.
  in
  Printf.printf "%s\n" (display false total); (* total amount *)
  Buffer.output_buffer stdout buffer;         (* record list *)
  Printf.printf "\n%!";                       (* end marker *)
  Buffer.clear buffer

(* The slave is launched by the GUI. It receives instructions on its standard input
   channel and prints results on its standard output channel. *)

let rec loop () =
  let line = input_line stdin in
  match instruction (Lexing.from_string line) with
  | IQuit ->
      ()
  | IDateRange (date1, date2) ->
      selectPortion date1 date2;
      spit();
      loop()
  | ISelect key ->
      selectKey key;
      spit();
      loop()

let () =
  loop()

(* RANGE 2005-11-01 00:00:00 +0000 2005-12-31 00:00:00 +0000 *)

