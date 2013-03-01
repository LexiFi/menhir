
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

open Unix
open Cparse

(* calcule sizeof(type) en le faisant faire par gcc. *)

let types_file = open_out "/tmp/.csur_types.h"
    (* on accumule ici les declarations de types lors du parsing. *)
let _ = output_string types_file "/* declarations de types pour calculer les sizeof() dans csur. */\n"

let sizeof_memo = (Hashtbl.create 128 : (string, int) Hashtbl.t)
let offsetof_memo = (Hashtbl.create 128 : (string, int) Hashtbl.t)

exception Sizeof
exception Offsetof

let sizeof_ctype qtyp =
    let name = let buf = Buffer.create 128 in
	       (bufout_type_name buf qtyp "";
		Buffer.contents buf) in
    try
      Hashtbl.find sizeof_memo name
    with Not_found ->
    (flush types_file;
     let sizeof_file = open_out "/tmp/.csur_sizeof.c" in
     output_string sizeof_file "#include \"/tmp/.csur_types.h\"\n\nextern struct FILE *stdout;\n\nmain () {\n  printf (\"%d\\n\", sizeof (";
     output_string sizeof_file name;
     output_string sizeof_file "));\n  fflush (stdout);\n}\n";
     close_out sizeof_file;
     match system "gcc -o /tmp/.csur_sizeof /tmp/.csur_sizeof.c" with
	 WEXITED 0 ->
	 let p = open_process_in "/tmp/.csur_sizeof" in
	 let size = int_of_string (input_line p) in
	     let _ = close_process_in p in
		 (Hashtbl.add sizeof_memo name size;
		  size)
       | _ -> raise Sizeof)

let offsetof_ctype qtyp f =
    let name = let buf = Buffer.create 128 in
	       (bufout_type_name buf qtyp "";
		Buffer.add_string buf ",";
		Buffer.add_string buf f;
		Buffer.contents buf) in
    try
      Hashtbl.find offsetof_memo name
    with Not_found ->
    (flush types_file;
     let offsetof_file = open_out "/tmp/.csur_offsetof.c" in
     output_string offsetof_file "#include \"/tmp/.csur_types.h\"\n\nextern struct FILE *stdout;\n#define offsetof(_typ,_f) ((int) ((((char *)&((_typ *)0)->_f)) - (char *)0))\n\nmain () {\n  printf (\"%d\\n\", offsetof (";
     output_string offsetof_file name;
     output_string offsetof_file "));\n  fflush (stdout);\n}\n";
     close_out offsetof_file;
     match system "gcc -o /tmp/.csur_offsetof /tmp/.csur_offsetof.c" with
	 WEXITED 0 ->
	 let p = open_process_in "/tmp/.csur_offsetof" in
	 let offset = int_of_string (input_line p) in
	     let _ = close_process_in p in
		 (Hashtbl.add offsetof_memo name offset;
		  offset)
       | _ -> raise Offsetof)

(** Ici, une fonction pour calculer les tailles des enums. **)

let sizeof_enum lenum =

 let rec output_string_enum chnout l =
  match l with
    [] -> ()
  | [(name, None)]      -> output_string chnout name
  | [(name, Some init)] -> output_string chnout (name ^" = "^(string_of_int (int_of_float init))) 
  | (name, None)::t -> (output_string chnout (name^","); output_string_enum chnout t)
  | (name, Some init)::t ->
      (output_string chnout (name ^" = "^(string_of_int (int_of_float init))^",");
       output_string_enum chnout t)
 in
 (
     flush types_file;
     let sizeof_file = open_out "/tmp/.csur_sizeof.c" in
     output_string sizeof_file "#include \"/tmp/.csur_types.h\"\n\nextern struct FILE *stdout;\n\nint main () {\n  printf (\"%d\\n\", sizeof (";
     
     (* output_string sizeof_file "enum  {";
     output_string_enum sizeof_file lenum;
     output_string sizeof_file "} ";
*)
     output_string sizeof_file " char ";
     output_string sizeof_file "));\n  fflush (stdout);\n}\n";
     close_out sizeof_file;
     match system "gcc -o /tmp/.csur_sizeof /tmp/.csur_sizeof.c" with
	 WEXITED 0 ->
	 let p = open_process_in "/tmp/.csur_sizeof" in
	 let size = int_of_string (input_line p) in
	     let _ = close_process_in p in size
       | _ -> raise Sizeof
)
