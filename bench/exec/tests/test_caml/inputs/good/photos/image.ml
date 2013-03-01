(* $Header: /home/yquem/cristal/fpottier/cvs/photos/image.ml,v 1.8 2005/12/11 15:43:47 fpottier Exp $ *)

open Printf
open Util

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Rotating an image. *)

type direction =
  | Left
  | Right

let rotate direction filename =
  tempcopy filename (fun newname ->
    let degrees = match direction with Left -> "270" | Right -> "90" in
    let qfile = Filename.quote filename
    and qnew = Filename.quote newname in
    command (sprintf "jpegtran -rotate %s -copy all -outfile %s %s" degrees qnew qfile);
    command (sprintf "/bin/mv -f %s %s; /bin/touch %s" qnew qfile qfile)
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Determining an image's resolution. *)

let resolution filename =
  let commande = sprintf "identify %s" (Filename.quote filename) in
  match oneliner commande with
  | Some result ->
      let regexp = Str.regexp ".*JPEG \\([0-9]+\\)x\\([0-9]+\\) " in
      if Str.string_match regexp result 0 then begin
        Some (int_of_string (Str.matched_group 1 result), int_of_string (Str.matched_group 2 result))
      end
      else
        None
  | None ->
      None

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Determining an image's creation date. *)

(* TEMPORARY interfacer directement libexif? *)

let regexp =
  Str.regexp "Date/Time[^:]*: \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)"

let creationdate filename =
  match multiliner (sprintf "jhead %s" (Filename.quote filename)) with
  | None ->
      moddate filename
  | Some text  ->
      try
	let _ = Str.search_forward regexp text 0 in
	let tm = {
	  Unix.tm_year = int_of_string (Str.matched_group 1 text) - 1900;
	  Unix.tm_mon = int_of_string (Str.matched_group 2 text) - 1;
	  Unix.tm_mday = int_of_string (Str.matched_group 3 text);
	  Unix.tm_hour = int_of_string (Str.matched_group 4 text);
	  Unix.tm_min = int_of_string (Str.matched_group 5 text);
	  Unix.tm_sec = int_of_string (Str.matched_group 6 text);
	  Unix.tm_wday = 0;
	  Unix.tm_yday = 0;
	  Unix.tm_isdst = false
	} in
	fst (Unix.mktime tm)
      with Not_found ->
	moddate filename

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Determining an image's size in kilobytes (rounded up). *)

let kilo bytes =
  if bytes mod 1024 = 0 then
    bytes / 1024
  else
    bytes / 1024 + 1

let size filename =
  kilo (Unix.stat filename).Unix.st_size

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Creating a postcard-like thumbnail out of an image. *)

(** Postcard size. *)
let geometry =
  "200x200"

(** Border size. *)
let border = 
  "8x8"

(** Maximum rotation, in degrees. *)
let rotation =
  5

(** Random seed. *)
let _ =
  Random.self_init()

(** Creating a postcard out of an image. The postcard is a reduced, framed, rotated
    version of the original image. *)
let postcard filename postcardname =

  tempcopy filename (fun tempname ->

    let qtemp = Filename.quote tempname
    and qpost = Filename.quote postcardname in

    let angle = Random.int (2*rotation+1) - rotation in

    command (sprintf "mogrify -size %s -resize %s -border %s -rotate %d -format jpg +profile '*' %s"
				    geometry geometry border angle qtemp);

    command (sprintf "jpegtran -progressive %s > %s" qtemp qpost)

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Creating a reduced (scaled down, compressed) version of an image.
   [geometry] is as expected by [mogrify] and must be quoted.
   [quality] is an integer between 0 and 100. *)

let reduce geometry quality filename dstname =

  tempcopy filename (fun tempname ->

    let qtemp = Filename.quote tempname
    and qdst = Filename.quote dstname in

    command (sprintf "mogrify -size %s -resize %s -format jpg -quality %d +profile '*' %s"
				    geometry geometry quality qtemp);

    command (sprintf "jpegtran -progressive %s > %s" qtemp qdst)

  )

