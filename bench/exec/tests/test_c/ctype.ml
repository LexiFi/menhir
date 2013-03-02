
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

(** Mise a jour par Fabrice Parrennes. **)

open Error
open Cparse
open Sizeof
open Arith

let global_types = (Hashtbl.create 512 : (string, var_declaration) Hashtbl.t)
    (* table des types de tous les identificateurs globaux. *)
let local_types_stack = ref [global_types] (* pile des tables de types *)

let context = ref ([] : string list)
    (* Toutes les fonctions et variables statiques, disons x, sont renommees en filename.x *)

exception BadAmpersand of expr

(** La fonction [push_types_stack] empile un ensemble de definitions. *)

let push_types_stack () =
  try
    (local_types_stack := Hashtbl.create 512 :: !local_types_stack)
  with err ->
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : push_types_stack : "^(Printexc.to_string err)) err in
    raise err

(** La fonction [pop_types_stack] depile un ensemble de definitions. *)

let pop_types_stack () =
  try
    local_types_stack := List.tl (!local_types_stack)
  with err ->
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : pop_types_stack : "^(Printexc.to_string err)) err in
    raise err

(** **)

let rec var_type_1 x l =
      match l with
        [] -> raise Not_found
      | tbl::rest ->
	    (
             try Hashtbl.find tbl x, rest=[]
	     with Not_found -> var_type_1 x rest)

let var_type_2 loc x =
  try
    begin
      match var_type_1 x (!local_types_stack) with
	CDECL (_, (_, name, qtyp)), glob ->
	  qtyp, x, glob
      | CFUN (_, (_, name, qtyp), _), glob ->
	  qtyp, x, glob
    end
  with err ->
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : var_type_2 : "^(Printexc.to_string err)^" on "^x) err in
    raise err

let varname s =
    try let start = String.rindex s '.' + 1 in
            String.sub s start (String.length s-start)
    with Not_found -> s;;

let rec var_type_aux loc x ctx =
    try var_type_2 loc x
    with Not_found -> match ctx with
			  [] -> (error (Some loc) ("undeclared variable " ^ varname x);
				 (((false, false), TINT)), x, true)
			| s::ctx' -> var_type_aux loc (s ^ "." ^ x) ctx'


let find_declaration loc x ctx =
  
  let find_var_type_2 loc x =
    try
      begin
        fst (var_type_1 x (!local_types_stack))
      end
    with err -> raise err

  in
  let rec find_var_type_aux loc x ctx =
    try find_var_type_2 loc x
    with Not_found -> match ctx with
			  [] -> raise Not_found
			| s::ctx' -> find_var_type_aux loc (s ^ "." ^ x) ctx'
  in  
  find_var_type_aux loc x ctx


let var_type loc x =

(* DEBUG *)   (*
(* DEBUG *)    print_string "\nAppel de var_type : ";
(* DEBUG *)    print_string "\nLe contexte = "; List.iter (fun x -> print_string (" "^x)) (!context);
(* DEBUG *)    print_string ("\nLa variable = "^x); 
    
(* DEBUG *)    let nb = ref 0 in
(* DEBUG *)    print_string "\nDans les tables = ";
(* DEBUG *)    List.iter (fun x -> 
(* DEBUG *)      nb := !nb +1;
(* DEBUG *)      print_string ("\nTable "^(string_of_int !nb));
(* DEBUG *)      Hashtbl.iter (fun y _ -> print_string ("\n  Var : "^y)) x)  (!local_types_stack);
(* DEBUG *)    *)     

    let old_contexte = 
      begin
        if (!context = []) then []
        else
          List.tl (!context) 
      end
    in 

    try
      begin
        match (find_declaration loc x old_contexte) with
          ( CFUN ( b, (_, _, _), _)) -> 
            begin
              if b then 
                begin
                  var_type_aux loc x old_contexte
                end
              else
                var_type_aux loc x (!context)
            end
        | ( CDECL ( _, (_, _, _))) -> 
            begin
              var_type_aux loc x (!context)
            end
    end
    with err ->
      begin
          match err with (** C'est comme l'ancienne fonction. **)
            Not_found -> var_type_aux loc x (!context)
          | _ -> 
              (* DEBUG*) let _ = Debug.eRROR_DEBUG ("var_type : "^(Printexc.to_string err)^" on "^x) err in
              raise err
      end
   

let rec is_function x = (* ou x est un nom qualifie; par exemple, si f est une
			 fonction static dans le fichier F, alors x est f.F *)
    try (match var_type_1 x (!local_types_stack) with
	     CFUN (_, (_, _, qtyp), _), _ -> Some qtyp
	   | CDECL (_, (_, _, ((_, TFUN _) as qtyp))), _ -> Some qtyp
           | _ -> None)
    with Not_found -> None

let declare_fun_1 x decl =
    Hashtbl.add global_types x decl

  (** La fonction "static_name_aux f ctx" permet de construire un nom de fonction
    lorsque celle-ci est déclarée en static. **)

let rec static_name_aux f ctx =
  try
    begin
      match ctx with
        [] -> f
      | s::ctx' -> 
          begin
            static_name_aux (s ^ "." ^ f) ctx'
          end
    end
  with err ->
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : static_name_aux : "^(Printexc.to_string err)) err in
    raise err

let static_name f = 
  let s = static_name_aux f (!context)  in
  s

let declare_fun static loc f qtyp code =
  try
    begin
      let name = if static then static_name f else f in
      let decl = CFUN (static, (loc, f, qtyp), code) in
      ((try (match var_type_1 f [global_types] with
        CDECL (cq, (loc', _, qtyp')), _ ->
	  ((if qtyp<>qtyp'
	  then 
            (
             error (Some loc) ("type of " ^ f ^ " does not match earlier declaration qtype = »"^(string_of_qtyp qtyp)^"« is not equal to »"^(string_of_qtyp qtyp')^"«")
            )
	  else if cq=CQ_STATIC && not static
	  then error (Some loc) (f ^ " was declared static but is not")
	  else if cq<>CQ_STATIC && static
	  then error (Some loc) (f ^ " is static but was declared non-static")
	  else ());
	   declare_fun_1 name decl)
      | CFUN (_, (loc', _, _), _), _ ->
	  (error (Some loc) ("definition of function " ^ f ^ " shadows earlier declaration");
	   declare_fun_1 name decl))
      with Not_found -> declare_fun_1 name decl);
       decl)
    end
  with err ->
(* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : static_name_aux : "^(Printexc.to_string err)) err in
             raise err

let declare_var_1 x decl =
  try    
    Hashtbl.add (List.hd (!local_types_stack)) x decl
  with err -> 
    failwith ("Ctype : static_name_aux : "^(Printexc.to_string err))

let declare_var cq loc x qtyp =
  try
    begin
      let name = if cq=CQ_STATIC then static_name x else x in
      let stack = if cq=CQ_DEFAULT then !local_types_stack else [global_types] in
      let decl = CDECL (cq, (loc, x, qtyp)) in
      ((try (match var_type_1 x stack with
	CDECL (cq', (loc', _, qtyp')), _ ->
	  ((if cq'<>CQ_EXTERN
	  then error (Some loc) ("illegal redeclaration of " ^ x)
	  else if qtyp<>qtyp'
	  then error (Some loc) ("type of " ^ x ^ " does not match extern declaration")
	  else ());
	   declare_var_1 name decl)
      | CFUN (_, (loc', _, _), _), _ ->
	  (error (Some loc) ("declaration of " ^ x ^ " shadows earlier declaration");
	   declare_var_1 name decl))
      with Not_found -> declare_var_1 name decl);
       decl)
    end
  with err -> 
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : declare_var : "^(Printexc.to_string err)) err in
    raise err


let string_cst_type s =
  try
    begin
      ((true, false), TARRAY (((true, false), TCHAR),
			      Some (String.length s + 1)))
       (* si la chaine est de longueur n, son type
       est const char [n+1] const. *)
    end
  with err -> 
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : string_cst_type : "^(Printexc.to_string err)) err in
    raise err

let err_not_number loc qtyp =
  try
    begin
      error (Some loc)
	(let buf = Buffer.create 128 in
	(Buffer.add_string buf "expected numeric type, got type ";
	 bufout_qtype buf qtyp;
	 Buffer.contents buf))
    end
  with err -> 
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : err_not_number : "^(Printexc.to_string err)) err in
    raise err
  
let cast_to_number loc (stor, typ) =
  try
    begin
      match typ with
	TINT | TUNSIGNED_INT | TCHAR | TUNSIGNED_CHAR |
	TSHORT | TUNSIGNED_SHORT | TLONG | TUNSIGNED_LONG |
	TFLOAT | TDOUBLE | TLONG_DOUBLE -> ()
      | _ -> err_not_number loc (stor, typ)
    end
  with err -> 
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : cast_to_number : "^(Printexc.to_string err)) err in
    raise err

let rec mk_convert typ' ((loc, ((stor, typ) as qtyp), e) as loce) =
  try
    begin
      if typ=typ'
      then loce
      else match typ', typ with
        TFLOAT, TDOUBLE 
      | TDOUBLE, TFLOAT 
      | TDOUBLE, TLONG 
      | TLONG, TDOUBLE 
      | TLONG, TUNSIGNED_LONG
      | TUNSIGNED_LONG, TLONG 
      | TINT, TUNSIGNED_INT 
      | TUNSIGNED_INT, TINT 
      | TSHORT, TUNSIGNED_SHORT 
      | TUNSIGNED_SHORT, TSHORT 
      | TCHAR, TUNSIGNED_CHAR 
      | TUNSIGNED_CHAR, TCHAR 
      |	TLONG, TINT 
      | TINT, TLONG 
      | TUNSIGNED_LONG, TUNSIGNED_INT 
      | TUNSIGNED_INT, TUNSIGNED_LONG 
      |	TLONG, TSHORT 
      | TSHORT, TLONG 
      |	TUNSIGNED_LONG, TUNSIGNED_SHORT 
      | TUNSIGNED_SHORT, TUNSIGNED_LONG 
      |	TLONG, TCHAR 
      | TCHAR, TLONG 
      |	TUNSIGNED_LONG, TUNSIGNED_CHAR 
      | TUNSIGNED_CHAR, TUNSIGNED_LONG 
      |	TINT, TSHORT 
      | TSHORT, TINT 
      |	TUNSIGNED_INT, TUNSIGNED_SHORT 
      | TUNSIGNED_SHORT, TUNSIGNED_INT 
      |	TINT, TCHAR 
      | TCHAR, TINT 
      |	TUNSIGNED_INT, TUNSIGNED_CHAR 
      | TUNSIGNED_CHAR, TUNSIGNED_INT 
      |	TSHORT, TCHAR 
      | TCHAR, TSHORT 
      |	TUNSIGNED_SHORT, TUNSIGNED_CHAR 
      | TUNSIGNED_CHAR, TUNSIGNED_SHORT -> (loc, (stor, typ'), CONVERT loce)

      (* FABRICE : comment faire pour le cast d'un type : TLONG_DOUBLE *)

      | TFLOAT, _          -> (loc, (stor, typ'), CONVERT (mk_convert TDOUBLE loce))

      | TDOUBLE, _         -> (loc, (stor, typ'), CONVERT (mk_convert TLONG loce))

      | TLONG, _           -> (loc, (stor, typ'), CONVERT (mk_convert TUNSIGNED_LONG loce))
(* Fabrice : Attention, il y a un cycle ..
      | TUNSIGNED_LONG, _  -> (loc, (stor, typ'), CONVERT (mk_convert TLONG loce))
*)
      | TSHORT, _          -> (loc, (stor, typ'), CONVERT (mk_convert TUNSIGNED_SHORT loce))
(* Fabrice : Attention, il y a un cycle ..
      | TUNSIGNED_SHORT, _ -> (loc, (stor, typ'), CONVERT (mk_convert TSHORT loce))
*)
      | TINT, _            -> (loc, (stor, typ'), CONVERT (mk_convert TUNSIGNED_INT loce))
(* Fabrice : Attention, il y a un cycle ..
      | TUNSIGNED_INT, _   -> (loc, (stor, typ'), CONVERT (mk_convert TINT loce))
*)
      | TCHAR, _           -> (loc, (stor, typ'), CONVERT (mk_convert TUNSIGNED_CHAR loce))
(* Fabrice : Attention, il y a un cycle ..
      | TUNSIGNED_CHAR, _  -> (loc, (stor, typ'), CONVERT (mk_convert TCHAR loce))
*)
      | _ -> (err_not_number loc (stor, typ'); (loc, (stor, typ'), CONVERT loce))
    end
  with err -> 
(* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : mk_convert : "^(Printexc.to_string err)) err in
            raise err

let convert_to_number ((loc, ((_, typ) as qtyp), _) as loce) =
  try
    begin
      match typ with
	TINT | TSHORT | TCHAR -> mk_convert TINT loce
      | TUNSIGNED_INT | TUNSIGNED_SHORT | TUNSIGNED_CHAR -> mk_convert TUNSIGNED_INT loce
      | TLONG | TUNSIGNED_LONG -> loce
      | TFLOAT | TDOUBLE -> mk_convert TDOUBLE loce
      | _ -> (err_not_number loc qtyp; mk_convert TINT loce)
    end
  with err -> 
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : convert_to_number : "^(Printexc.to_string err)) err in
    raise err
   

let convert_to_integer loce =
  try
    begin
      let (_, (_, typ), _) as loce' = convert_to_number loce in
      ((match typ with
	TFLOAT | TDOUBLE -> error (Some (loc_of_expr loce))
	    "cannot use floating-point type here"
      | _ -> ());
       loce')
    end
  with err ->
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : convert_to_integer : "^(Printexc.to_string err)) err in
    raise err
    
let convert_to_int ((loc, ((stor, typ) as qtyp), e) as loce) =
  try
    begin
      (cast_to_number loc qtyp;
       mk_convert TINT loce)
    end
  with err ->
    (* DEBUG *) let _ = Debug.eRROR_DEBUG ("Ctype : convert_to_int : "^(Printexc.to_string err)) err in
    raise err


let convert_to_long ((loc, ((stor, typ) as qtyp), e) as loce) =
    (cast_to_number loc qtyp;
     mk_convert TLONG loce)

let cast_to_float loc (stor, typ) =
    match typ with
	TINT | TCHAR | TSHORT | TLONG | TFLOAT | TDOUBLE -> ()
      | _ -> error (Some loc)
	(let buf = Buffer.create 128 in
	     (Buffer.add_string buf "expected floating point type, got type ";
	      bufout_qtype buf (stor, typ);

	      Buffer.contents buf))

let rec field_type loc qtyp f =
    let (stor, typ) = qtyp in
    match typ with
        TSTRUCT sname ->
	(try
	 let fields = Hashtbl.find structs sname in
	     (try
	      let (_, _, qtyp') = List.find (fun (_, f', _) -> f=f') fields
		  in qtyp'
	      with Not_found ->
		   (error (Some loc) ("struct " ^ sname ^ " has no field named " ^ f);
		    ((false, false), TINT)))
	     with Not_found -> (error (Some loc) ("undefined struct " ^ sname);
				((false, false), TINT)))
      | TUNION sname ->
	(try
	 let fields = Hashtbl.find unions sname in
	     (try
	      let (_, _, qtyp') = List.find (fun (_, f', _) -> f=f') fields
		  in qtyp'
	      with Not_found ->
		   (error (Some loc) ("union " ^ sname ^ " has no field named " ^ f);
		    ((false, false), TINT)))
	     with Not_found -> (error (Some loc) ("undefined union " ^ sname);
				((false, false), TINT)))
      | _ -> (error (Some loc)
	      (let buf = Buffer.create 128 in
		   (Buffer.add_string buf "expected struct or union type, got type ";
		    bufout_qtype buf qtyp;
		    Buffer.contents buf));
		   ((false, false), TINT))

exception PrepareDeref

let prepare_deref_1 ((loc, qtyp, _) as loce) =
    let (stor, typ) = qtyp in
    match typ with
	TPOINTER ((_, t) as qt) when t<>TVOID -> qt, loce
      | TARRAY (qt, _) -> qt, (loc, (stor, TPOINTER qt), CONVERT loce)
      | _ -> raise PrepareDeref

let prepare_deref ((loc, ((stor, _) as qtyp), _) as loce) =
    try prepare_deref_1 loce
    with PrepareDeref -> (error (Some loc)
			  (let buf = Buffer.create 128 in
			       (Buffer.add_string buf "expected non-void pointer type, got type ";
				bufout_qtype buf qtyp;
				Buffer.contents buf));
			       ((false, false),  TPOINTER (stor, TCHAR)), loce)

let rec mk_amp e =
    match e with
	VAR (x, glob) -> ADDR_VAR (x, glob)
      | PTR (_, _, a) -> a
      | _ -> raise (BadAmpersand e)

let mk_address_of_1 loc' (loc, qtyp, e) =
    loc', ((false, false), TPOINTER qtyp), mk_amp e

let mk_address_of loc (loc', qtyp, e) =
    try
    loc, ((false, false), TPOINTER qtyp), mk_amp e
    with BadAmpersand _ -> (error (Some loc) "not an lvalue";
			    loc, ((false, false), TPOINTER qtyp),
			    CAST (loc, ((true, false), TINT), CST (INT 0.0)))

let rec mk_index loc loce i =
    let j = convert_to_long i in
    let qt, ((_, _, e) as loce') = prepare_deref loce in
    loc, qt, PTR (loc, qtyp_of_expr loce, ADDR_INDEX (loce', j))

let rec mk_ptr loc loce =
    let qt, ((_, _, e) as loce') = prepare_deref loce in
    loc, qt,
    match e with
        ADDR_VAR (x, glob) -> VAR (x, glob)
      | _ -> PTR loce

let mk_sel loc ((locs, qtyp, a) as s) f =
    (* On compile a.f par *(&a)->f. *)
  
(* DEBUG *) (*   Debug.pRINT_DEBUG2 ("\nmk_sel sur »"^(string_of_expr a)^"."^f^"«"); *)
(* DEBUG *) (*   Debug.pRINT_DEBUG2 ("\nLe type = "^(string_of_qtyp qtyp)); *)
(* DEBUG *) (*   flush stdout; *)
    
(* DEBUG *)    (** FABRICE.                                                    **)
(* DEBUG *)    (** Ici, il faut faire attention a la déclaration du f.         **)
(* DEBUG *)    (** Si f est a l'interieur d'un type union "union", il faut lui **)
(* DEBUG *)    (** ajouter union.f **)

    let ftype = field_type loc qtyp f in
    
(* DEBUG     Debug.pRINT_DEBUG2 ("\n Le type de »"^f^"« == "^(string_of_qtyp ftype)); *)
(* DEBUG     flush stdout; *)

(* DEBUG *)    (** FABRICE, on calcule bien l'offset par rapport au type "qtyp" **)
    let offset = 
      try
          offsetof_ctype  qtyp f
      with Sizeof.Offsetof -> 
        begin
(* DEBUG *)          (*
(* DEBUG *)          print_string "Erreur de génération du fichier .csur_offset";
(* DEBUG *)          let (_,a,b,c,d) = loc in
(* DEBUG *)          Error.prerr_locator ("qwerty",a,b,c,d);
(* DEBUG *)          print_string "\n\n";
(* DEBUG *)          flush stdout;
(* DEBUG *)          *)
          raise Sizeof.Offsetof 
        end
    in
(*	Code Original Jean, Non correct 
   mk_ptr loc (loc, ((false, false), TPOINTER qtyp),
		    ADDR_SEL (mk_address_of locs s, f, offset))

  Pourquoi ???

*)
     mk_ptr loc (loc, ((false, false), TPOINTER ftype),
		    ADDR_SEL (mk_address_of locs s, f, offset))



let expect_type loc' ((_, exp) as expected) ((loc, ((stor, typ) as qtyp), e) as loce) =

    if qtyp=expected
	then loce
    else match exp with
	     TINT | TCHAR | TSHORT | TLONG |
	     TUNSIGNED_INT | TUNSIGNED_CHAR | TUNSIGNED_SHORT | TUNSIGNED_LONG |
	     TFLOAT | TDOUBLE ->
	     ((match typ with
		   TINT | TCHAR | TSHORT | TLONG |
		   TUNSIGNED_INT | TUNSIGNED_CHAR | TUNSIGNED_SHORT | TUNSIGNED_LONG |
		   TFLOAT | TDOUBLE ->
		   ()
		 | _ -> error (Some loc')
		   (let buf = Buffer.create 128 in
			(Buffer.add_string buf "expected type ";
			 bufout_qtype buf expected;
			 Buffer.add_string buf ", got type ";
			 bufout_qtype buf qtyp;
			 Buffer.contents buf)));
		  mk_convert exp loce)
           | TFUN _ -> (error (Some loc') "cannot convert to function type (use a pointer to function instead)";
			loce)
           | TPOINTER ((_, (TFUN _ as exp')) as expected') ->
	     (match typ with
                  TPOINTER ((_, (TFUN _ as typ')) as qtyp') ->
		  (if qtyp'=expected'
		       then ()
		   else error (Some loc') "incompatible function types";
		       loce)
                | TPOINTER _ -> (error (Some loc') "incompatible pointer types";
				 (loc', expected, CAST loce))
		| _ -> error (Some loc') "assignment of non-pointer to pointer";
		  (loc', expected, CAST loce))

          | TPOINTER (_, TVOID) ->
	    (match typ with
	         TPOINTER _ -> (loc', expected, CAST loce)
	       | TARRAY (_, _) -> (loc', expected, CONVERT loce)
	       | _ -> (error (Some loc') "assignment of non-pointer to pointer";
		       (loc', expected, CAST loce)))

          | TPOINTER (_, exp') ->
	    (if (match typ with
		     TPOINTER (_, typ') -> exp'=typ'
		   | TARRAY ((_, typ'), _) -> exp'=typ'
		   | _ -> false)
		 then ()
	     else error (Some loc') "assignment of incompatible pointer types";
		 match typ with
		     TARRAY (qtyp', _) -> (loc', expected, CAST (loc, (stor, TPOINTER qtyp'),
								 CONVERT loce))
		   | _ -> (loc', expected, CAST loce))
          | TVOID -> (error (Some loc') "cannot assign to void"; loce)
          | _ -> (if typ=exp
		      then warning (Some loc') "some qualifiers have changed"
		  else error (Some loc') (let buf = Buffer.create 128 in
					      (Buffer.add_string buf "incompatible types, expected ";
					       bufout_qtype buf expected;
					       Buffer.add_string buf ", got ";
					       bufout_qtype buf qtyp;
					       Buffer.contents buf));
		      (loc', expected, CAST loce))

let cast_to_type loc' ((storx, exp) as expected) ((loc, ((stor, typ) as qtyp), e) as loce) =
    if qtyp=expected
	then loce
    else match exp with
	     TINT | TCHAR | TSHORT | TLONG |
	     TUNSIGNED_INT | TUNSIGNED_CHAR | TUNSIGNED_SHORT | TUNSIGNED_LONG |
	     TFLOAT | TDOUBLE ->
	     (match typ with
		   TINT | TCHAR | TSHORT | TLONG |
		   TUNSIGNED_INT | TUNSIGNED_CHAR | TUNSIGNED_SHORT | TUNSIGNED_LONG |
		   TFLOAT | TDOUBLE ->
		   mk_convert exp loce
		 | TARRAY (qtyp', _) ->
		   mk_convert exp (loc', (storx, TUNSIGNED_LONG),
				       CONVERT (loc', (stor, TPOINTER qtyp'), CONVERT loce))
		 | TPOINTER qtyp' ->
		   mk_convert exp (loc', (storx, TUNSIGNED_LONG), CONVERT loce)
		 | _ -> (error (Some loc')
			 (let buf = Buffer.create 128 in
			      (Buffer.add_string buf "cannot cast from type ";
			       bufout_qtype buf qtyp;
			       Buffer.add_string buf " to type ";
			       bufout_qtype buf expected;
			       Buffer.contents buf));
			      loce))
           | TPOINTER expected' ->
	     (match typ with
		  TINT | TCHAR | TSHORT | TLONG |
		  TUNSIGNED_INT | TUNSIGNED_CHAR | TUNSIGNED_SHORT | TUNSIGNED_LONG |
		  TFLOAT | TDOUBLE ->
		  loc', expected, CONVERT (mk_convert TUNSIGNED_LONG loce)
		| TPOINTER _ -> loc', expected, CONVERT loce
		| TARRAY (qtyp', _) ->
		  if qtyp'=expected'
		      then loc', expected, CONVERT loce
		  else loc', expected, CONVERT (loc', (storx, TPOINTER qtyp'), CONVERT loce)
		| _ -> (error (Some loc')
			(let buf = Buffer.create 128 in
			     (Buffer.add_string buf "cannot cast from type ";
			      bufout_qtype buf qtyp;
			      Buffer.add_string buf " to type ";
			      bufout_qtype buf expected;
			      Buffer.contents buf));
			     loce))
          | _ -> (error (Some loc')
			(let buf = Buffer.create 128 in
			     (Buffer.add_string buf "cannot cast from type ";
			      bufout_qtype buf qtyp;
			      Buffer.add_string buf " to type ";
			      bufout_qtype buf expected;
			      Buffer.contents buf));
			     loce)

let rec mk_set loc ((loc', qtyp, lval) as left) ((loc, ((stor, typ) as qtype), e) as rval) =

 (* DEBUG  *) Debug.pRINT_DEBUG2 "\nJe passe dans mk_set"; flush stdout; 
 (* DEBUG  *) Debug.pRINT_DEBUG2 ("\nL'expression "^(string_of_loc_expr lval)^" de type = "^(string_of_qtyp qtyp)); flush stdout;  
 (* DEBUG  *) Debug.pRINT_DEBUG2 ("\nDoit etre egale a "); 
 (* DEBUG  *) Debug.pRINT_DEBUG2 (" l'expression "^(string_of_loc_expr e)^" = "^(string_of_qtyp qtype)); flush stdout;  

 (* DEBUG *) (** FABRICE : Il faut corriger le probleme du traitement de : **)

    let rval' = expect_type loc qtyp rval in
	match qtyp with
	    ( _ , TSTRUCT _ ) | ( _ , TUNION _ ) ->
	    loc, qtyp, COPY (mk_address_of loc left,
			     mk_address_of loc' rval)

	  | _ ->
	    match lval with
		VAR (x, glob) -> 
                  begin
                    (* DEBUG *) Debug.pRINT_DEBUG2 "\nmk_set cas VAR";

                    loc, qtyp, SET_VAR (x, rval', glob)
                  end
	      | PTR a -> 
                  begin
                    (* DEBUG *) Debug.pRINT_DEBUG2 "\nmk_set cas PTR";
                    loc, qtyp, SET_PTR (a, rval')
                  end
(* FABRICE *) | FUN x ->
(* FABRICE *)     begin
(* FABRICE *)       (* DEBUG *) Debug.pRINT_DEBUG2 "\nmk_set cas FUN";
(* FABRICE *)       loc, qtyp, SET_VAR (x, rval', true)
(* FABRICE *)     end
(* FABRICE, Ici a priori une fonction est par defaut globale en "C". *)
	      | _ -> 
                  begin
                    (* DEBUG *) Debug.pRINT_DEBUG2 "\nmk_set cas Err\n";
                    error (Some loc) "not an lvalue";
		    rval
                  end

(* Note: affectation;
 f = g lorsque f est une variable de *pointeur* de fonction et g est une fonction: correct,
 se compile en f = &g.
 Utiliser expect_type. *)

let rec apply_args ell loc params args =
    match params, args with
	[], [] -> []
      | [], _ -> (if ell
		      then args
		  else (error (Some loc) "too many arguments"; []))
      | _, [] -> (error (Some loc) "not enough arguments"; [])
      | (_, x, qtyp_param)::params', arg::args' ->
	expect_type (loc_of_expr arg) qtyp_param arg :: apply_args ell loc params' args'

let mk_apply_ind loc ((loc', ((_, ftyp) as qftyp), f) as func) args =
    match f with
	PTR locf ->
	(match ftyp with
	     TFUN (T_PROTOTYPE (params, ell), qres) ->
	     (loc, qres, CALL_IND (locf, qftyp, apply_args ell loc params args))
	   | _ -> (error (Some loc) "application of non-function";
		   (loc', ((true, false), TINT), CST (INT 0.0))))
      | _ -> (error (Some loc') "application of object that is not a pointer to a function";
	      (loc, ((true, false), TINT), CST (INT 0.0)))

let mk_apply loc ((loc', _, f) as func) args =
    match f with
        FUN fname -> (match is_function fname with
			  Some (_, TFUN (T_PROTOTYPE (params, ell), qres)) ->
			  (loc, qres, CALL (fname, apply_args ell loc params args))
			| Some _ -> (error (Some loc') "application of non-function";
					   (loc, ((false, false), TINT), CST (INT 0.0)))
			| None -> mk_apply_ind loc func args)
     | _ -> mk_apply_ind loc func args

exception SupNumType

(*
 sup_num_type: quand on combine un unsigned et un signed,
 le resultat est signed.
 *)

let sup_num_type typ1 typ2 =
    match typ1 with
	TLONG ->
	(match typ2 with
	     TINT | TUNSIGNED_INT | TCHAR | TUNSIGNED_CHAR |
	     TSHORT | TUNSIGNED_SHORT | TLONG | TUNSIGNED_LONG -> TLONG
	   | TFLOAT | TDOUBLE -> TDOUBLE
	   | _ -> raise SupNumType)
      | TUNSIGNED_LONG ->
	(match typ2 with
	     TINT | TCHAR |
	     TSHORT | TLONG -> TLONG
	   | TUNSIGNED_INT | TUNSIGNED_CHAR |
	     TUNSIGNED_SHORT | TUNSIGNED_LONG -> TUNSIGNED_LONG
	   | TFLOAT | TDOUBLE -> TDOUBLE
	   | _ -> raise SupNumType)
      | TINT | TCHAR | TSHORT ->
	(match typ2 with
	     TLONG | TUNSIGNED_LONG -> TLONG
	   | TINT | TUNSIGNED_INT | TCHAR | TUNSIGNED_CHAR |
	     TSHORT | TUNSIGNED_SHORT -> TINT
	   | TFLOAT | TDOUBLE -> TDOUBLE
	   | _ -> raise SupNumType)
      | TUNSIGNED_INT | TUNSIGNED_CHAR | TUNSIGNED_SHORT ->
	(match typ2 with
	     TLONG -> TLONG
	   | TUNSIGNED_LONG -> TUNSIGNED_LONG
	   | TINT | TCHAR |
	     TSHORT -> TINT
	   | TUNSIGNED_INT | TUNSIGNED_CHAR |
	     TUNSIGNED_SHORT -> TUNSIGNED_INT
	   | TFLOAT | TDOUBLE -> TDOUBLE
	   | _ -> raise SupNumType)
      | TFLOAT | TDOUBLE ->
	(match typ2 with
	     TINT | TUNSIGNED_INT | TCHAR | TUNSIGNED_CHAR |
	     TSHORT | TUNSIGNED_SHORT | TLONG | TUNSIGNED_LONG |
	     TFLOAT | TDOUBLE -> TDOUBLE
	   | _ -> raise SupNumType)
      | _ -> raise SupNumType

let convert_num_sup ((loc1, ((cst1, vol1), typ1), e1) as loce1)
                    ((loc2, ((cst2, vol2), typ2), e2) as loce2) =
    let stor = (cst1 && cst2), (vol1 && vol2) in
    let typ = sup_num_type typ1 typ2 in
	 (stor, typ),
	 mk_convert typ loce1,
	 mk_convert typ loce2

let convert_int_sup loce1 loce2 =
    let ((_, typ), _, _) as res = convert_num_sup loce1 loce2 in
	((match typ with
	      TFLOAT | TDOUBLE -> raise SupNumType
	    | _ -> ());
	     res)

let mk_add loc loce1 loce2 =
    try (let typ, loce'1, loce'2 = convert_num_sup loce1 loce2 in
	     loc, typ, OP (S_ADD, loce'1, loce'2)
	     )
    with SupNumType ->
    (try let _, loce'1 = prepare_deref_1 loce1 in
         let loce'2 = convert_to_long loce2 in
	     loc, qtyp_of_expr loce'1, ADDR_INDEX (loce'1, loce'2)
     with PrepareDeref ->
    (try let _, loce'2 = prepare_deref_1 loce2 in
         let loce'1 = convert_to_long loce1 in
	     loc, qtyp_of_expr loce'2, ADDR_INDEX (loce'2, loce'1)
     with PrepareDeref ->
	  (error (Some loc) "cannot add non-numerical types";
	   loc, qtyp_of_expr loce1, OP (S_ADD, loce1, loce2))))

let mk_post_incdec loc inc ((loc', ((_, typ) as qtyp), e) as loce) =
    match e with
        VAR (x, glob) -> (* x++ est (y=x, x=y+1, y) *)
	      let y = gensym ".y" in
		  loc, qtyp, ESEQ [(loc', qtyp, SET_VAR (y, loce, false));
				       (let (_, qtyp', _) as loce' =
					    mk_add loc loce (loc, ((true, false), TINT),
							     CST (INT (if inc then 1.0 else -1.0)))
					in mk_set loc loce loce');
					    (loc, qtyp, VAR (y, false))]
     | _ -> (* sinon loce++ est (p=&loce, y=*p, *p=y+1, y) *)
	      let p = gensym ".p" in
	      let y = gensym ".y" in
	      let (_, qtyp', _) as loce' = mk_address_of loc loce in
              let loce'' = mk_ptr loc' (loc', qtyp', VAR (p, false)) in
		  loc, qtyp, ESEQ [(loc', qtyp', SET_VAR (p, loce', false));
				       (loc, qtyp, SET_VAR (y, loce'', false));
				       (loc, qtyp, SET_PTR ((loc', qtyp', VAR (p, false)),
								mk_add loc (loc, qtyp, VAR (y, false))
								(loc, ((true, false), TINT),
								 CST (INT (if inc then 1.0 else -1.0)))));
				       (loc, qtyp, VAR (y, false))]

let mk_if loc locif locthen locelse =
    let locif' = convert_to_int locif in
    try (let typ, loce'1, loce'2 = convert_num_sup locthen locelse in
	     loc, typ, EIF (locif', loce'1, loce'2)
	     )
    with SupNumType ->
	 ((if qtyp_of_expr locthen<>qtyp_of_expr locelse
	       then error (Some loc) "incompatible types on branches of conditional"
	   else ());
	       loc, qtyp_of_expr locthen, EIF (locif', locthen, locelse))

let mk_minus loc ((_, qtyp, _) as loce) =
    (cast_to_number loc qtyp;
     loc, qtyp, MINUS loce)

let mk_bang loc loce =
    mk_if loc loce (loc, ((true, false), TINT), CST (INT 0.0))
    (loc, ((true, false), TINT), CST (INT 1.0))

let mk_and loc loce1 loce2 =
    mk_if loc loce1 loce2 (loc, ((true, false), TINT), CST (INT 0.0))

let mk_or loc loce1 loce2 =
    mk_if loc loce1 (loc, ((true, false), TINT), CST (INT 1.0)) loce2 

let mk_bnot loc loce =
    let loce' = convert_to_integer loce in
    loc, qtyp_of_expr loce', BNOT loce'

let mk_setop setop loc loce1 loce2 =
 (* DEBUG *) let _ = Debug.pRINT_DEBUG2 "Appel de mk_setop " in
    match setop with
	S_ADD -> mk_add loc loce1 loce2
      | S_SUB -> (let (_, (_, typ), _) = loce2 in
		  match typ with
	              TINT | TUNSIGNED_INT | TCHAR | TUNSIGNED_CHAR |
	              TSHORT | TUNSIGNED_SHORT | TLONG | TUNSIGNED_LONG |
	              TFLOAT | TDOUBLE ->
		      mk_add loc loce1 (mk_minus (loc_of_expr loce2) loce2)
                    | _ -> (* soustraction de pointeurs, normalement. *)
		      let qt1, loce'1 = prepare_deref loce1 in
	              let qt2, loce'2 = prepare_deref loce2 in
			  (if qt1=qt2
			       then ()
			   else error (Some loc) "subtraction of pointers of different types";
			       loc, ((false, false), TLONG), ANTI_ADDR_INDEX (loce'1, loce'2)))
      | S_MUL | S_DIV ->
	(try (let typ, loce'1, loce'2 = convert_num_sup loce1 loce2 in
		  loc, typ, OP (setop, loce'1, loce'2)
		  )
         with SupNumType -> (error (Some loc) "arithmetic operation on non-numerical types";
			     loc, (qtyp_of_expr loce1), OP (setop, loce1, loce2)))
      | S_MOD ->
	(try (let typ, loce'1, loce'2 = convert_int_sup loce1 loce2 in
		  loc, typ, OP (setop, loce'1, loce'2)
		  )
         with SupNumType -> (error (Some loc) "modulo (%) on non-integer types";
			     loc, (qtyp_of_expr loce1), OP (setop, loce1, loce2)))
      | S_LEFT | S_RIGHT ->
	let (_, qtyp, _) as loce'1 = convert_to_integer loce1 in
	let loce'2 = convert_to_int loce2 in
	    loc, qtyp, OP (setop, loce'1, loce'2)
      | S_AND | S_XOR | S_OR ->
	(try (let typ, loce'1, loce'2 = convert_int_sup loce1 loce2 in
		  loc, typ, OP (setop, loce'1, loce'2)
		  )
	 with SupNumType -> (error (Some loc) "bitwise logical operation on non-integer types";
			     loc, (qtyp_of_expr loce1), OP (setop, loce1, loce2)))
      | S_EQ -> (error (Some loc) "illegal S_EQ setop in mk_setop (internal error)";
		 loce1)

let mk_set_setop setop loc ((loc1, qtyp, e1) as loce1) loce2 = (* l += e, etc. *)
(* DEBUG *) let _ = Debug.pRINT_DEBUG2 "mk_set_setop : main" in

    match setop with
        S_EQ -> 
          begin
             (* DEBUG *) let _ = Debug.pRINT_DEBUG2 "mk_set_setop car S_EQ" in
            mk_set loc loce1 loce2
          end
      | _ -> begin
               (* DEBUG *) let _ = Debug.pRINT_DEBUG2 "mk_set_setop car <> S_EQ" in
          match e1 with
            VAR (_, _) -> (* x+=e est x=x+e *)
	      mk_set loc loce1 (mk_setop setop loc loce1 loce2)
	  | _ -> (* sinon l+=e est (p=&l, *p=*p+e) *)
	      let p = gensym ".p" in
	      let (_, qtyp', _) as loce' = mk_address_of loc loce1 in
	      let loce'' = mk_ptr loc1 (loc1, qtyp', VAR (p, false)) in
              (* DEBUG *) let _ = Debug.pRINT_DEBUG2 "mk_set_setop ..." in
              
	      loc, qtyp,
	      ESEQ [(loc, qtyp', SET_VAR (p, loce', false));
		    (loc, qtyp, SET_PTR ((loc1, qtyp', VAR (p, false)),
					 mk_setop setop loc loce'' loce2))]
      end                

let mk_set_add loc loce1 loce2 = 
  (* DEBUG *) Debug.pRINT_DEBUG2 "Appel de mk_set_add";
  mk_set_setop S_ADD loc loce1 loce2

let mk_pre_incdec loc inc loce = (* ++l, --l equivalent a l += 1, l += -1 *)
    mk_set_add loc loce (loc, ((true, false), TINT),
			 CST (INT (if inc then 1.0 else -1.0)))

let mk_cmp cmpop loc loce1 loce2 =
  (* DEBUG *) Debug.pRINT_DEBUG2 "Appel de mk_cmp";
    match cmpop with
        C_EQ -> (try let _, loce'1, loce'2 = convert_num_sup loce1 loce2 in
	             loc, ((false, false), TINT), CMP (C_EQ, loce'1, loce'2)
		 with SupNumType ->
		      ((if qtyp_of_expr loce1<>qtyp_of_expr loce2
			    then error (Some loc) "incompatible types as arguments of == or !="
			else match snd (qtyp_of_expr loce1) with
				 TSTRUCT _ | TUNION _ | TFUN _ ->
				 error (Some loc) "cannot compare structures, unions or functions"
			       | _ -> ());
			    loc, ((false, false), TINT), CMP (C_EQ, loce1, loce2)))
     | C_LT | C_LE -> (try let _, loce'1, loce'2 = convert_num_sup loce1 loce2 in
		           loc, ((false, false), TINT), CMP (cmpop, loce'1, loce'2)
		       with SupNumType ->
			    (error (Some loc) "arithmetic comparison on non-numerical types";
			     loc, ((false, false), TINT), CMP (cmpop, loce1, loce2)))

let mk_eseq loc loce1 loce2 =
    loc, (qtyp_of_expr loce2), ESEQ [loce1; loce2]
	
