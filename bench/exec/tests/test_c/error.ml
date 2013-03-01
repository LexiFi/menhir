(*
 *	Copyright (c) 2002 by Laboratoire Spécification et Vérification (LSV),
 *	CNRS UMR 8643 & ENS Cachan.
 *	Written by Jean Goubault-Larrecq.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. Neither the author nor its employer is responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 *	4. This software is restricted to non-commercial use only.  Commercial
 *		use is subject to a specific license, obtainable from LSV.
 *)

type locator = string * int * int * int * int
    (* nom du fichier, ou "";
     premiere ligne,
     premiere colonne,
     derniere ligne,
     derniere colonne.
     *)

let sup_locator (file, line1, col1, _, _) (file', _, _, line2, col2) =
    ((if file="" then file' else file),
	 line1, col1, line2, col2)

type hlocator = string * int * int

let loc_start (file, line1, col1, _, _) = (file, line1, col1)
let loc_end (file, _, _, line2, col2) = (file, line2, col2)

let prerr_locator (file, line1, col1, line2, col2) =
    if file=""
	then ()
    else (prerr_string file;
	  prerr_string ", line";
	  (if line1<>line2 then prerr_string "s" else ());
	       prerr_string " ";
	       prerr_int line1;
	       (if col1<>0 then (prerr_string "("; prerr_int col1; prerr_string ")") else ());
		    (if line1<>line2 || col1<>col2
			 then (prerr_string "-";
			       prerr_int line2;
			       (if col2<>0 then (prerr_string "("; prerr_int col2;
						 prerr_string ")") else ()))
		     else ()))

let prerr_loc loc = match loc
    with Some l -> (prerr_locator l; prerr_string ": ")
	 | _ -> ()

let warning loc msg =
    (
(* DEBUG *) if (Debug.is_error ()) then
       begin
         prerr_string "parser: ";
         prerr_loc loc;
         prerr_endline msg
       end
    )

let error_count = ref 0
let error_count_max = 10000


let fatal loc msg =
    (warning loc msg; exit 10)

let flush_error () =
    if !error_count>=error_count_max
	 then fatal None "Too many errors: quit"
     else ()

let error loc msg =
    (error_count := !error_count + 1;
     warning loc msg;
    if !error_count>=error_count_max
	 then fatal loc "Too many errors: quit"
     else ())

let gensym_count = ref 0
let gensym prefix =
	(incr gensym_count;
	let s = string_of_int (!gensym_count) in
	prefix ^ s)

let hash_replace h x y =
    (Hashtbl.remove h x;
     Hashtbl.add h x y)

