open Printf

type punctuation =
    Buffer.t -> unit

type 'a printer =
    Buffer.t -> 'a -> unit

(* ------------------------------------------------------------------------- *)

(* Newlines and indentation. *)

let maxindent =
  120

let whitespace =
  String.make maxindent ' '

let indentation =
  ref 0

let nl buffer =
  Buffer.add_char buffer '\n';
  Buffer.add_substring buffer whitespace 0 !indentation

let indent ofs producer buffer x =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + ofs in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  bprintf buffer "%t%a" nl producer x;
  indentation := old_indentation

(* ------------------------------------------------------------------------- *)

(* Lists. *)

let rec list elem buffer xs =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      bprintf buffer "%a%a" elem x (list elem) xs

let rec preclist delim elem buffer xs =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      bprintf buffer "%t%a%a" delim elem x (preclist delim elem) xs

let rec termlist delim elem buffer xs =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      bprintf buffer "%a%t%a" elem x delim (termlist delim elem) xs

let seplist sep elem buffer xs =
  match xs with
  | [] ->
      ()
  | x :: xs ->
      bprintf buffer "%a%a" elem x (preclist sep elem) xs

let annlist announcement list buffer xs =
  match xs with
  | [] ->
      ()
  | _ :: _ ->
      bprintf buffer "%t%a" announcement list xs

(* ------------------------------------------------------------------------- *)

(* Punctuation. *)

let space buffer =
  bprintf buffer " "

let comma buffer =
  bprintf buffer ", "

let wedge buffer =
  bprintf buffer " /\\ "

let vee buffer =
  bprintf buffer " \\/ "

let semicolon buffer =
  bprintf buffer "; "

let var buffer =
  bprintf buffer "var "

let seminl buffer =
  bprintf buffer "%t%t" semicolon nl

let nlspace k buffer =
  bprintf buffer "%t%s" nl (String.make k ' ')

let nlnl buffer =
  bprintf buffer "%t%t" nl nl

(* ------------------------------------------------------------------------- *)

(* [firstthen first then] first prints [first], then keeps printing
   [then]. *)

let firstthen f t =
  let first = ref true in
  function buffer ->
    if !first then begin
      f buffer;
      first := false
    end
    else
      t buffer

(* [atmost n delimiter stop] normally prints a [delimiter], except that,
   every [n] calls, it prints a [stop] in addition. *)

let atmost n (delimiter : punctuation) (stop : punctuation) : punctuation =
  let i =
    ref 0
  in
  function buffer ->
    incr i;
    delimiter buffer;
    if !i = n then begin
      i := 0;
      stop buffer
    end

(* ------------------------------------------------------------------------- *)

(* Tables. *)

let width column =
  List.fold_left (fun width x ->
    max width (String.length x)
  ) 0 column

let pad width x =
  let y = String.make width ' ' in
  String.blit x 0 y 0 (String.length x);
  y

let pad column =
  List.map (pad (width column)) column

let rec zipcat column1 column2 =
  List.fold_right2 (fun x1 x2 column ->
    (x1 ^ x2) :: column
  ) column1 column2 []

let catenate columns =
  match columns with
  | [] ->
      []
  | column :: columns ->
      List.fold_left (fun table column ->
	zipcat table (pad column)
      ) (pad column) columns

let transposerev lines =
  match lines with
  | [] ->
      []
  | line :: lines ->
      List.fold_left (fun columns line ->
	List.fold_right2 (fun x column columns ->
	  (x :: column) :: columns
        ) line columns []
      ) (List.map (fun x -> [ x ]) line) lines

(* ------------------------------------------------------------------------- *)

(* [wrap] turns a buffer printer into a channel printer. *)

let wrap bp channel x =
  let buffer = Buffer.create 2048 in
  bp buffer x;
  Buffer.output_buffer channel buffer

(* [w] is a specialised form of [wrap] that prints on the standard
   error channel. *)

let w f =
  wrap (fun b () ->
    f b
  ) stderr ()

