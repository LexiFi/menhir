(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: constraintPrettyPrinter.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module implements a pretty printer for constraints. *)

open Positions
open Misc
open PrettyPrinter
open Constraint
open Print
open MultiEquation
open Format

(** The constraint over equality between terms. *)
type formula = 
    (MultiEquation.crterm, MultiEquation.variable) Constraint.type_constraint

let print_crterm ?user_name_from_int t =
  print_term ?user_name_from_int false t

let print_variable ?user_name_from_int v =
  let vt = print_variable ?user_name_from_int false v in
  if (UnionFind.find v).structure <> None then
    Printf.sprintf "(%s)" vt
  else 
    vt

let active_mode mode = 
  match mode with
    | Formatter r ->
	Format.set_tags true;
	set_all_formatter_output_functions 
	    ~out:r.out
	    ~flush:r.flush 
	    ~newline:r.newline 
	    ~spaces:r.spaces ;
	if r.with_tags then (
	  set_formatter_tag_functions
	    {
	      mark_open_tag   = (fun t -> r.open_tag t; "");
	      mark_close_tag  = (fun t -> r.close_tag t; "");
	      print_open_tag  = ignore;
	      print_close_tag = ignore;
	    };
	  set_margin r.margin)

    | Latex out -> 
	let _ = Format.set_margin 115 in
	let outputf s p n =
	  let s = (String.sub s p n) in
	  let s' = 
	    List.fold_left 
	      (fun acu (s, s') -> 
		 Str.global_replace (Str.regexp_string s) s' acu)
	      s [ 
		("\\", "$\\partial$");
		("let ", "\\textbf{let} ");
		("in ", "\\textbf{in} ");
		("=>", "$\\Rightarrow$");
		("*", "$\\times$");
		("forall", "$\\forall$"); 
		("exists", "$\\exists$"); 
		("_", "");
		("and", "$\\wedge$");
		(" or ", " $\\vee$ ");
		("->", " $\\rightarrow$ ");
		("true", "$\\top$");
		("<", "$<$"); 
		(">", "$>$"); 
		("=", "$\\stackrel{?}{=}$"); 
	      ] in
	    output_string out s'
	and flush () = flush out
	and newline () = output_string out "\\\\\n"
	and spaces n =
	  if (n != 0) then
	    output_string out (Printf.sprintf "\\hspace*{%fcm}" (float_of_int n *. 0.02))
	in
	set_all_formatter_output_functions 
	  ~out:outputf 
	  ~flush:flush 
	  ~newline:newline 
	  ~spaces:spaces 

    | Txt out -> 	  
	let _ = Format.set_margin 80 in
	  set_all_formatter_output_functions 
	    ~out:(fun s b e -> output_string out (String.sub s b e))
	    ~flush:(fun () -> flush out) 
	    ~newline:(fun () -> output_string out "\n") 
	    ~spaces:(fun n -> for i = 0 to n do output_string out " " done)

let is_let = function 
  | CLet _ -> true 
  | _      -> false

let paren f =
  print_string "(";
  f ();
  print_string ")"

let printf_constraint 
    ?forall ?exists ?andsym ?before ?after ?user_name_from_int mode c = 
  let exists = default "exists " exists
  and forall = default "forall " forall 
  and andsym = default "and" andsym 
  and print_variable = print_variable ?user_name_from_int
  and print_crterm = print_crterm ?user_name_from_int 
  in

  let rec pconstraint c = 
    ignore (opt_apply before c); 
    (match c with
       | CTrue _ 
       | CConjunction []
       | CDisjunction [] -> 
	   print_string "true"

       | CDump _ -> 
	   print_string "dump"

       | CEquation (_, t1, t2) -> 
	   printf "%s = %s"
	     (print_crterm t1)
	     (print_crterm t2)

       | CConjunction (c :: []) -> 
	   pconstraint c

       | CConjunction (c :: cs) -> 
	   printf "(";
	   pconstraint c;
	   List.iter (fun c -> 
			printf "@ %s@ "
			  andsym;
			if is_let c then paren 
			  (fun () -> pconstraint c) else pconstraint c) cs;
	   printf ")"

       | CDisjunction (c :: []) -> 
	   pconstraint c

       | CDisjunction (c :: cs) -> 
	   assert false

       | CLet ([ Scheme (_, [], fqs, c, h) ], CTrue _) 
	   when StringMap.empty = h -> 
	   let rec chop_exists acu = function
	     | CLet ([ Scheme (_, [], fqs', c', _) ], CTrue _) -> 
		 chop_exists (acu @ fqs') c'
	     | lc -> (acu, lc)
	   in
	     let (fqs, c) = chop_exists fqs c in 
	       if (List.length fqs <> 0) then
		 print_string exists;
	       print_string (print_separated_list " " print_variable fqs);
	       if (List.length fqs <> 0) then
		 printf ".@,";
	       printf "@[<b 2>";
	       pconstraint c; 
	       printf "@]"

       | CLet (schemes, c) -> 
	   printf "let @[<b>";
	   printf_schemes schemes;
	   printf "@]@ in@ @[<b>";
	   pconstraint c;
	   printf "@]"

       | CInstance (_, SName name, t) -> 
	   printf "%s < %s" name (print_crterm t)
    );
    ignore (opt_apply after c) 


  and printf_schemes  = function
    | [] -> ()
    | [ x ] -> printf_scheme x
    | x :: q -> (printf_scheme x; print_cut (); print_string " ; "; 
		 print_cut (); printf_schemes q)

  and is_true = function CTrue _ -> true | _ -> false 

  and printf_scheme (Scheme (_, rqs, fqs, c, header)) = 
    let len = StringMap.fold (fun x k acu -> acu + 1) header 0 in
      printf "";
      if (List.length rqs + List.length fqs <> 0) then
	print_string forall;
      print_string (print_separated_list " " print_variable fqs);
      if rqs <> [] then 
	printf "{%s}" (print_separated_list " " print_variable rqs);
      if not (is_true c) then (
	printf "@,[@[<b>";
	pconstraint c;
	printf "@]]");
      if (len <> 0) then print_string "(";
      let f = ref true in
      let sep () = if !f then (f := false; "") else "; " in
	StringMap.iter (fun name (t, pos) -> printf "%s%s : %s"
			  (sep ())  name (print_crterm t))
	  header;
	if (len <> 0) then print_string ")";


  in (
      active_mode mode;
      open_box 0;
      pconstraint c;
      close_box ();
      print_newline ();
    )

