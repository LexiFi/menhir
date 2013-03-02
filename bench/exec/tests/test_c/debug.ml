(*
 *	Copyright (c) 2002 by Laboratoire Spécification et Vérification (LSV),
 *	CNRS UMR 8643 & ENS Cachan.
 *	Written by Fabrice Parrennes.  Not derived from licensed software.
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

(** Ce fichier définit une fonction d'affichage des debugs **)

(*DEBUG*) let eRROR_DEBUG s e =
(*DEBUG*)  print_string ("\n"^s);
(*DEBUG*)  flush stdout;
(*DEBUG*)  raise e

(*DEBUG*) let eRROR_DEBUG s e = raise e

(*DEBUG*) let pRINT_DEBUG s = 
(*DEBUG*)      print_string ("\n"^s); 
(*DEBUG*)      flush stdout

(*DEBUG*) let pRINT_DEBUG s = ()


(*DEBUG*) let pRINT_DEBUG2 s = 
(*DEBUG*)      print_string ("\n"^s); 
(*DEBUG*)      flush stdout

(*DEBUG*) let pRINT_DEBUG2 s = ()

(*DEBUG*) let pRINT_WARNING s =
(*DEBUG*)      print_string ("\n"^s); 
(*DEBUG*)      flush stdout

(*DEBUG*) let pRINT_WARNINGs s = ()

(*DEBUG*) let val_error = ref true

(*DEBUG*) let set_error b = val_error := b

(*DEBUG*) let is_error () = !val_error

(*DEBUG*) let pRINT_DEBUG_PARSE s = 
(*DEBUG*)      print_string ("\n"^s); 
(*DEBUG*)      flush stdout

(*DEBUG*) let pRINT_DEBUG_PARSE s = ()
