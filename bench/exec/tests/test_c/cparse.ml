
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
 * 
 * Update: 
 * - 13/09/2002 : ajout du type TLONG_DOUBLE
 *
 *
*)

open Error

type cstorage = bool * bool (* const flag, volatile flag *)

type ctype = TVOID
  | TINT
  | TUNSIGNED_INT
  | TCHAR
  | TUNSIGNED_CHAR
  | TSHORT
  | TUNSIGNED_SHORT
  | TLONG
  | TUNSIGNED_LONG
  | TFLOAT
  | TDOUBLE
  | TLONG_DOUBLE
  | TSTRUCT of string (* le nom de la struct. *)
  | TUNION of string (* le nom de l'union. *)
  | TPOINTER of q_ctype (* les type "T *", "const T *", "volatile T *",
			 "const volatile T *". *)
  | TARRAY of q_ctype * int option (* int [20] est TARRAY (TINT, Some 20),
				    int [] est TARRAY (TINT, None) *)
  | TFUN of param_ctype * q_ctype


and q_ctype = cstorage * ctype

and param_ctype = T_PROTOTYPE of (locator * string * q_ctype) list * bool
    (* liste de parametres, avec endroit de definition, nom du parametre
     (la chaine vide "" si aucun nom n'a ete fourni), le type du parametre;
     + booleen qui est true si la liste de parametres se termine par "...";
     une declaration "int f ()" est consideree comme equivalente a "int f (...)". *)

type constant_val = INT of float
    | FLOAT of float
    | STRING of string

type set_op = S_EQ | S_MUL | S_DIV | S_MOD | S_ADD | S_SUB | S_LEFT |
    S_RIGHT | S_AND | S_XOR | S_OR
    (* Note: S_EQ et S_SUB ne pourront jamais apparaitre dans une expr. *)

type cmp_op = C_LT | C_LE | C_EQ

type loc_expr = locator * q_ctype * expr

and expr = VAR of string * bool (* une variable; true si globale, false si locale. *)
    | ARRAY of string * bool (* un tableau; true si global, false si local. *)
    | FUN of string (* une fonction; toujours globale. *)
    | CST of constant_val (* une constante *)
    | PTR of loc_expr (* *a *)
    | SET_VAR of string * loc_expr * bool (* affectation x=e;
					   bool=true si x globale, false si x locale. *)
    | SET_PTR of loc_expr * loc_expr (* affectation *a = e *)
    | COPY of loc_expr * loc_expr (* copie le contenu de la deuxieme expr
				   (une struct ou une union) dans la premiere. *)
    | ADDR_VAR of string * bool (* &x, ou x est une variable;
				 bool=true si x globale, false si x locale. *)
    | ADDR_INDEX of loc_expr * loc_expr (* &a[i] *)
    | ANTI_ADDR_INDEX of loc_expr * loc_expr (* calcule a-b, pour a et b deux
					      pointeurs vers le meme type. *)
    | ADDR_SEL of loc_expr * string * int (* &a->champ; le dernier entier est l'offset *)
    | CALL of string * loc_expr list (* f(e1,...,en) *)
    | CALL_IND of loc_expr * q_ctype * loc_expr list (* ( *e) (e1,...,en) *)
    | CONVERT of loc_expr (* (type)e, quand il s'agit d'une conversion,
				     par ex. (int)e. *)
    | CAST of loc_expr (* (type)e, quand il s'agit d'un cast, par ex. (char * )e. *)
	(* operations logiques: *)
    | BNOT of loc_expr (* ~e; a ne pas confondre avec !e *)
	(* operations arithmetiques: *)
    | MINUS of loc_expr (* -e *)
    | OP of set_op * loc_expr * loc_expr (* multiplication, addition, etc. *)
    | CMP of cmp_op * loc_expr * loc_expr (* e < e', e<=e', e==e' *)
    | EIF of loc_expr * loc_expr * loc_expr (* e1?e2:e3 *)
    | ESEQ of loc_expr list (* e1, ..., en [sequence, analogue a e1;e2 au niveau code];
			     si n=0, represente skip. *)

type field_ctype = locator * string * q_ctype
type cqualifier = CQ_DEFAULT | CQ_EXTERN | CQ_STATIC
type var_declaration = CDECL of cqualifier * field_ctype
  | CFUN of bool * field_ctype * loc_code (* definition de fonction,
					   le bool est vrai si static. *)
and loc_code = locator * code
and code =
    CLABEL of string * loc_code
  | CCASE of locator * constant_val * loc_code
  | CDEFAULT of loc_code
  | CBLOCK of var_declaration list * loc_code list
  | CEXPR of loc_expr
  | CIF of loc_expr * loc_code * loc_code
  | CSWITCH of loc_expr * loc_code
  | CLOOP of loc_code * loc_code
  | CGOTO of string
  | CCONTINUE
  | CBREAK
  | CRETURN of loc_expr option

let cline = ref 1
let ccol = ref 0
let oldcline = ref 1
let oldccol = ref 0
let cfile = ref ""

let getloc () = (!cfile, !oldcline, !oldccol, !cline, !ccol)

let typedefs = (Hashtbl.create 100 : (string, q_ctype) Hashtbl.t) (* table des typedefs *)
let structs  = (Hashtbl.create 100 : (string, field_ctype list) Hashtbl.t) (* table des structs *)
let unions   = (Hashtbl.create 20  : (string, field_ctype list) Hashtbl.t) (* table des unions *)

(** FABRICE **)

(** Les enums sont gérés comme des Tables de Hash (Nom -> (Valeur, Taille)) **)

let enums = (Hashtbl.create 50  : (string,(float*int)) Hashtbl.t) 

(** **)

let loc_of_expr (loc, _, _) = loc
let qtyp_of_expr (_, qtyp, _) = qtyp

let index_prec  = 15 (* a[i] *)
let ptr_prec    = 15 (* a->f *)
let dot_prec    = 15 (* a.f *)
let bang_prec   = 14 (* !a *)
let tilde_prec  = 14 (* ~a *)
let incdec_prec = 14 (* ++a, a++, --a, a-- *)
let cast_prec   = 14 (* (T)a *)
let sizeof_prec = 14 (* sizeof T *)
let uplus_prec  = 14 (* +a *)
let uminus_prec = 14 (* -a *)
let star_prec   = 14 (* *a *)
let amper_prec  = 14 (* &a *)
let mul_prec    = 13 (* a*b *)
let div_prec    = 13 (* a/b *)
let mod_prec    = 13 (* a%b *)
let add_prec    = 12 (* a+b *)
let sub_prec    = 12 (* a-b *)
let shift_prec  = 11 (* a<<b, a>>b *)
let cmp_prec    = 10 (* a<b, a<=b, a>b, a>=b *)
let eq_prec     = 9 (* a==b, a!=b *)
let binand_prec = 8 (* a & b *)
let binxor_prec = 7 (* a ^ b *)
let binor_prec  = 6 (* a | b *)
let and_prec    = 5 (* a && b *)
let or_prec     = 4 (* a || b *)
let if_prec     = 3 (* a?b:c *)
let setop_prec  = 2 (* a += b, a *= b, ... *)
let comma_prec  = 1 (* a, b *)

let bufout_delim buf pri newpri s =
    if newpri<pri
	then Buffer.add_string buf s
    else ()

let bufout_open buf pri newpri = bufout_delim buf pri newpri "("
let bufout_close buf pri newpri = bufout_delim buf pri newpri ")"

let setop_text setop =
    match setop with
	S_EQ -> "="
      | S_MUL -> "*="
      | S_DIV -> "/="
      | S_MOD -> "%="
      | S_ADD -> "+="
      | S_SUB -> "-="
      | S_LEFT -> "<<="
      | S_RIGHT -> ">>="
      | S_AND -> "&="
      | S_XOR -> "^="
      | S_OR -> "|="

let op_text setop =
    match setop with
	S_EQ -> ""
      | S_MUL -> "*"
      | S_DIV -> "/"
      | S_MOD -> "%"
      | S_ADD -> "+"
      | S_SUB -> "-"
      | S_LEFT -> "<<"
      | S_RIGHT -> ">>"
      | S_AND -> "&"
      | S_XOR -> "^"
      | S_OR -> "|"

let op_prec setop =
    match setop with
	S_EQ -> 0
      | S_MUL -> mul_prec
      | S_DIV -> div_prec
      | S_MOD -> mod_prec
      | S_ADD -> add_prec
      | S_SUB -> sub_prec
      | S_LEFT -> shift_prec
      | S_RIGHT -> shift_prec
      | S_AND -> binand_prec
      | S_XOR -> binxor_prec
      | S_OR -> binor_prec

type typical_use =
    TU_VAR of string
  | TU_PTR of typical_use * cstorage
  | TU_ARR of typical_use * cstorage * int option
  | TU_FUN of typical_use * cstorage * param_ctype

let rec typical_use (stor, typ) tu =
  match typ with
    TPOINTER qtyp'    -> 
      begin

(* DEBUG *)   (** Fabrice : Ici regardons s'il ne faut pas inverser FUN et PTR ?? **)
(* DEBUG *)   (** Selon Jean, NON => c'est donc a voir. **)

        (* DEBUG *) let _ = Debug.pRINT_DEBUG "typical_use TPOINTER" in
        match tu with
          TU_FUN (tufun,cst,param) ->
            begin
              typical_use qtyp' (TU_FUN ((TU_PTR (tufun, stor)) , cst,param))
            end
        | _ -> typical_use qtyp' (TU_PTR (tu, stor))
      end
  | TARRAY (qtyp', n) -> 
      begin
        (* DEBUG *) let _ = Debug.pRINT_DEBUG "typical_use TARRAY" in
        typical_use qtyp' (TU_ARR (tu, stor, n))
      end
  | TFUN (args, res)  -> 
      begin
        (* DEBUG *) let _ = Debug.pRINT_DEBUG "typical_use TFUN" in
        typical_use res   (TU_FUN (tu, stor, args))
      end
  | _                 -> (stor, typ), tu
        
let rec string_of_tu tu =
  match tu with
    TU_VAR s        -> ("var "^s)
  | TU_PTR (tu,cs)  -> ("Ptr "^(string_of_tu tu))
  | TU_ARR (tu,_,_) -> ("Arr "^(string_of_tu tu))
  | TU_FUN (tu,_,_) -> ("Fun "^(string_of_tu tu))
      

exception BufOutDecl of ctype

let rec bufout_expr buf pri e =
    match e with
	VAR (s, _) -> Buffer.add_string buf s
      | ARRAY (s, _) -> Buffer.add_string buf s
      | FUN s -> Buffer.add_string buf s
      | CST (INT n) -> Buffer.add_string buf (string_of_float n)
      | CST (FLOAT x) -> Buffer.add_string buf (string_of_float x)
      | CST (STRING s) -> Buffer.add_string buf (String.escaped s)
      | PTR a -> (bufout_open buf pri star_prec;
		  Buffer.add_string buf "*";
		  bufout_loc_expr buf index_prec a;
		  bufout_close buf pri star_prec)
      | SET_VAR (x, e, _) -> (bufout_open buf pri setop_prec;
			      Buffer.add_string buf x;
			      Buffer.add_string buf "=";
			      bufout_loc_expr buf setop_prec e;
			      bufout_close buf pri setop_prec)
      | SET_PTR (a, e) ->
	(bufout_open buf pri setop_prec;
	 bufout_open buf setop_prec star_prec;
	 Buffer.add_string buf "*";
	 bufout_loc_expr buf star_prec a;
	 bufout_close buf setop_prec star_prec;
	 Buffer.add_string buf "=";
	 bufout_loc_expr buf setop_prec e;
	 bufout_close buf pri setop_prec)
      | COPY (from, tgt) ->
	(bufout_open buf pri setop_prec;
	 bufout_open buf setop_prec star_prec;
	 Buffer.add_string buf "*";
	 bufout_loc_expr buf star_prec from;
	 bufout_close buf setop_prec star_prec;
	 Buffer.add_string buf "=";
	 bufout_open buf setop_prec star_prec;
	 Buffer.add_string buf "*";
	 bufout_loc_expr buf star_prec tgt;
	 bufout_close buf setop_prec star_prec;
	 bufout_close buf pri setop_prec)
      | ADDR_VAR (s, _) -> (Buffer.add_string buf "&";
			    Buffer.add_string buf s)
      | ADDR_SEL (a, f, offset) -> (bufout_open buf pri amper_prec;
				    Buffer.add_string buf "&";
				    bufout_loc_expr buf ptr_prec a;
				    Buffer.add_string buf "->";
				    Buffer.add_string buf f;
				    bufout_close buf pri amper_prec)
      | ADDR_INDEX (a, i) -> (bufout_open buf pri amper_prec;
			      Buffer.add_string buf "&";
			      bufout_open buf amper_prec index_prec;
			      bufout_loc_expr buf index_prec a;
			      Buffer.add_string buf "[";
			      bufout_loc_expr buf 0 i;
			      Buffer.add_string buf "]";
			      bufout_close buf amper_prec index_prec;
			      bufout_close buf pri amper_prec)
      | ANTI_ADDR_INDEX (a, b) -> (bufout_open buf pri sub_prec;
				   bufout_loc_expr buf sub_prec a;
				   Buffer.add_string buf "-";
				   bufout_loc_expr buf sub_prec b;
				   bufout_close buf pri sub_prec)
      | CALL (f, l) -> (bufout_open buf pri index_prec;
			Buffer.add_string buf f;
			Buffer.add_string buf "(";
			bufout_loc_expr_list buf l;
			Buffer.add_string buf ")";
			bufout_close buf pri index_prec)
      | CALL_IND (f, _, l) -> (bufout_open buf pri index_prec;
			       bufout_loc_expr buf index_prec f;
			       Buffer.add_string buf "(";
			       bufout_loc_expr_list buf l;
			       Buffer.add_string buf ")";
			       bufout_close buf pri index_prec)
      | CAST e -> bufout_loc_expr buf pri e
      | CONVERT e -> bufout_loc_expr buf pri e
      | BNOT e -> (bufout_open buf pri tilde_prec;
		  Buffer.add_string buf "~";
		  bufout_loc_expr buf tilde_prec e;
		  bufout_close buf pri tilde_prec)
      | MINUS e -> (bufout_open buf pri uminus_prec;
		    Buffer.add_string buf "-";
		    bufout_loc_expr buf uminus_prec e;
		    bufout_close buf pri uminus_prec)
      | OP (setop, e, e') -> let newpri = op_prec setop in
	(bufout_open buf pri newpri;
	 bufout_loc_expr buf newpri e;
	 Buffer.add_string buf (op_text setop);
	 bufout_loc_expr buf newpri e';
	 bufout_close buf pri newpri)
      | CMP (C_LT, e, e') -> (bufout_open buf pri cmp_prec;
			      bufout_loc_expr buf cmp_prec e;
			      Buffer.add_string buf "<";
			      bufout_loc_expr buf cmp_prec e';
			      bufout_close buf pri cmp_prec)
      | CMP (C_LE, e, e') -> (bufout_open buf pri cmp_prec;
			      bufout_loc_expr buf cmp_prec e;
			      Buffer.add_string buf "<=";
			      bufout_loc_expr buf cmp_prec e';
			      bufout_close buf pri cmp_prec)
      | CMP (C_EQ, e, e') -> (bufout_open buf pri eq_prec;
			      bufout_loc_expr buf eq_prec e;
			      Buffer.add_string buf "==";
			      bufout_loc_expr buf eq_prec e';
			      bufout_close buf pri eq_prec)
      | EIF (eb, et, ee) -> (bufout_open buf pri if_prec;
			     bufout_loc_expr buf if_prec eb;
			     Buffer.add_string buf "?";
			     bufout_loc_expr buf if_prec et;
			     Buffer.add_string buf ":";
			     bufout_loc_expr buf if_prec ee;
			     bufout_close buf pri if_prec)
      | ESEQ (e::l) -> (bufout_open buf pri comma_prec;
			bufout_loc_expr buf comma_prec e;
			List.iter (fun e' -> (Buffer.add_string buf ",";
					      bufout_loc_expr buf comma_prec e')) l;
			bufout_close buf pri comma_prec)
      | ESEQ [] -> ()
and bufout_loc_expr buf pri (_, qtyp, e) =
  match e with
      CAST e' -> bufout_cast buf pri qtyp e'
    | CONVERT e' -> bufout_cast buf pri qtyp e'
    | _ -> bufout_expr buf pri e
and bufout_cast buf pri qtyp e =
    (bufout_open buf pri cast_prec;
     Buffer.add_string buf "(";
     bufout_qtype buf qtyp;
     Buffer.add_string buf ")";
     bufout_loc_expr buf cast_prec e;
     bufout_close buf pri cast_prec)
and bufout_loc_expr_list buf l =
    match l with
	[] -> ()
      | [a] -> bufout_loc_expr buf comma_prec a
      | a::l' -> (bufout_loc_expr buf comma_prec a;
		  Buffer.add_string buf ",";
		  bufout_loc_expr_list buf l')
and bufout_stor buf stor =
    ((if fst stor then Buffer.add_string buf "const " else ());
	  (if snd stor then Buffer.add_string buf "volatile " else ()))
and bufout_qtype buf qtyp = bufout_type_name buf qtyp ""

(** La fonction "bufout_type_name" est utilisée pour le calcul du
    sizeof. **)

and bufout_type_name buf qtyp var =

  (* DEBUG *) let _ = Debug.pRINT_DEBUG "appel de bufout_type_name" in

  let decl, tu = typical_use qtyp (TU_VAR var) in

  (* DEBUG *) let _ = Debug.pRINT_DEBUG (string_of_tu tu) in
  (* DEBUG *) let _ = Debug.pRINT_DEBUG "Ok appel de typical use" in
  
  (bufout_decl buf decl;
   Buffer.add_string buf " ";
   bufout_typical_use buf tu)


and bufout_decl buf (stor, typ) =
    (bufout_stor buf stor;
     match typ with
        TVOID           -> Buffer.add_string buf "void"
      | TINT            -> Buffer.add_string buf "int"
      | TUNSIGNED_INT   -> Buffer.add_string buf "unsigned int"
      | TCHAR           -> Buffer.add_string buf "char"
      | TUNSIGNED_CHAR  -> Buffer.add_string buf "unsigned char"
      | TSHORT          -> Buffer.add_string buf "short"
      | TUNSIGNED_SHORT -> Buffer.add_string buf "unsigned short"
      | TLONG           -> Buffer.add_string buf "long"
      | TUNSIGNED_LONG  -> Buffer.add_string buf "unsigned long"
      | TFLOAT          -> Buffer.add_string buf "float"
      | TDOUBLE         -> Buffer.add_string buf "double"
      | TLONG_DOUBLE    -> Buffer.add_string buf "long double"
      | TSTRUCT s       -> (Buffer.add_string buf "struct ";
		            Buffer.add_string buf s)
      | TUNION s        -> (Buffer.add_string buf "union ";
		            Buffer.add_string buf s)
      
      | _ -> raise (BufOutDecl typ))


and bufout_typical_use buf tu  =
    match tu with
        TU_VAR x -> 
          begin
            (* DEBUG *) let _ = Debug.pRINT_DEBUG "\nbufout_typical_use TU_VAR" in
            Buffer.add_string buf x
          end
      | TU_PTR (tu', stor) -> (
          (* FABRICE, 
             Ici, il faut regarder si c'est un nom de fonction.
             Attention au cas **.
          *)
          
          (* DEBUG *) let _ = Debug.pRINT_DEBUG "\nbufout_typical_use TU_PTR" in

          Buffer.add_string buf "*";
	  bufout_stor buf stor;
	  bufout_typical_use buf tu';
         )
      | TU_ARR (tu', _, n) -> 
          (* DEBUG *) let _ = Debug.pRINT_DEBUG "\nbufout_typical_use TU_ARR" in

          let paren = match tu' with TU_PTR _ -> true | TU_FUN _ -> true
						       | _ -> false in
			   (if paren then Buffer.add_string buf "(" else ();
			    bufout_typical_use  buf tu';
			    if paren then Buffer.add_string buf ")" else ();
			    Buffer.add_string buf "[";
			    (match n with
				 Some n' -> Buffer.add_string buf (string_of_int n')
			       | _ -> ());
			    Buffer.add_string buf "]")
      | TU_FUN (tu', _, params) -> (
          (

           (* DEBUG *) let _ = Debug.pRINT_DEBUG "\nbufout_typical_use TU_FUN" in

           match tu' with
	     TU_VAR x -> 
               begin
                   Buffer.add_string buf x
               end
           | _ -> 
               begin
                 Buffer.add_string buf "(";
		 bufout_typical_use  buf tu';
		 Buffer.add_string buf ")"
               end
          );
	  Buffer.add_string buf "(";
	  match params with
	    T_PROTOTYPE ([], true) -> Buffer.add_string buf ")";
	  | T_PROTOTYPE (args, ell) ->
              begin
	        bufout_param_types buf args;
	        if ell then Buffer.add_string buf ",..." else ();
	        Buffer.add_string buf ")"
              end
         )

and bufout_param_types buf args =
    match args with
        [] -> Buffer.add_string buf "void"
      | (loc, x,typ)::rest -> (bufout_type_name buf typ x;
			  match rest with
			      [] -> ()
		            | _ -> (Buffer.add_string buf ",";
				    bufout_param_types buf rest))

let rec bufout_fields_1 buf fields =
    match fields with
	[] -> Buffer.add_string buf "}"
      | (_, f, qtyp)::rest ->
	(Buffer.add_string buf "  ";
	 bufout_type_name buf qtyp f;
	 Buffer.add_string buf ";\n";
	 bufout_fields_1 buf rest)
let bufout_fields buf fields =
    match fields with
        None -> ()
      | Some flds -> (Buffer.add_string buf "{\n";
		      bufout_fields_1 buf flds)

let pr_qtype file qtyp =
    let buf = Buffer.create 128 in
	(bufout_qtype buf qtyp;
	 Buffer.output_buffer file buf)

let pr_type_name file qtyp var =
    let buf = Buffer.create 128 in
	(bufout_type_name buf qtyp var;
	 Buffer.output_buffer file buf)

(** FABRICE, des fonctions pour le debug. **)

let string_of_qtyp qtyp =
  let buf = Buffer.create 128 in
  bufout_qtype buf qtyp;
  Buffer.contents buf

let rec string_of_expr e =
  let buf = Buffer.create 128 in
  bufout_loc_expr buf comma_prec e;
  Buffer.contents buf

let rec string_of_loc_expr e =
  let buf = Buffer.create 128 in
  bufout_expr buf comma_prec e;
  Buffer.contents buf
