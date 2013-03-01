%{

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
 * Update :
 * - Le 13/09/2002 : mise a jour
 * 
*)

(* Analyse syntaxique de C.
   Traduit a partir d'un fichier yacc. (Jean Goubault-Larrecq, 2002.)
	Original quote:

   ANSI C Yacc grammar

   In 1985, Jeff Lee published his Yacc grammar (which is accompanied
   by a matching Lex specification) for the April 30, 1985 draft
   version of the ANSI C standard.  Tom Stockfisch reposted it to
   net.sources in 1987; that original, as mentioned in the answer to
   question 17.25 of the comp.lang.c FAQ, can be ftp'ed from
   ftp.uu.net, file usenet/net.sources/ansi.c.grammar.Z.

   I intend to keep this version as close to the current C Standard
   grammar as possible; please let me know if you discover
   discrepancies.

   Jutta Degener, 1995 
 *)

open Cparse
open Error
open Sizeof
open Arith
open Ctype

exception ExpectedStructOrUnion

exception NotConstantExpression of expr

let rec eval_constant_loc_expr ((loc, qtyp, e) as loce) =
	match e with
	CST v -> (qtyp, v)
      | CAST e -> convert qtyp (eval_constant_loc_expr e)
      | CONVERT e -> convert qtyp (eval_constant_loc_expr e)
	 (* Note: la classe de stockage stor est simplement ignoree. *)
      | BNOT e' -> calc_bnot (eval_constant_loc_expr e')
      | MINUS e' -> calc_minus (eval_constant_loc_expr e')
      | OP (S_ADD, e1, e2) -> calc_add (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_SUB, e1, e2) -> calc_sub (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_MUL, e1, e2) -> calc_mul (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_DIV, e1, e2) -> calc_div (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_MOD, e1, e2) -> calc_mod (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_LEFT, e1, e2) -> calc_left (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_RIGHT, e1, e2) -> calc_right (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_AND, e1, e2) -> calc_and (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_XOR, e1, e2) -> calc_xor (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | OP (S_OR, e1, e2) -> calc_or (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | CMP (C_LT, e1, e2) -> calc_lt (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | CMP (C_LE, e1, e2) -> calc_le (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | CMP (C_EQ, e1, e2) -> calc_eq (eval_constant_loc_expr e1) (eval_constant_loc_expr e2)
      | EIF (eif, ethen, eelse) ->
	if calc_is_zero (eval_constant_loc_expr eif)
	    then eval_constant_loc_expr eelse
	else eval_constant_loc_expr ethen
      | ESEQ l -> eval_constant_eseq l
      | _ -> raise (NotConstantExpression e)

and eval_constant_eseq l =
    match l with
        [e] -> eval_constant_loc_expr e
      | (e::rest) -> let _ = eval_constant_loc_expr e in
	               eval_constant_eseq rest
      | _ -> raise (NotConstantExpression (ESEQ l))

let parse_error msg =
    fatal (Some (getloc ())) msg

let mk_type (cst, vol) ((cst', vol'), typ) =
	(cst || cst, vol || vol'), typ

let return_type = ref ((false, false), TINT)

(** Fonctions, pour la gestion des enums **)

let numenums = ref 0.0
let resetenums () = numenums := 0.0

let taille = sizeof_enum [ ("TT",None) ]

let enum_add_hash  (v,ini) =
  match ini with
    None   ->
      (** Ici, pas de valeur d'initialisation, donc. **)
      begin
        Hashtbl.add enums v (!numenums, taille);
        numenums := !numenums +. 1.
      end
          
  | Some ninit ->
      (** Ici, il faut mettre a jour. **)
      begin
        numenums := ninit;
        Hashtbl.add enums v (ninit,taille);
        numenums := !numenums +. 1.
      end
%}

%token <string> IDENTIFIER TYPE_NAME
%token <Cparse.ctype * Cparse.constant_val> CONSTANT
%token <string> STRING_LITERAL
%token SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN
%token SEMI_CHR OPEN_BRACE_CHR CLOSE_BRACE_CHR COMMA_CHR COLON_CHR
%token EQ_CHR OPEN_PAREN_CHR CLOSE_PAREN_CHR OPEN_BRACKET_CHR
%token CLOSE_BRACKET_CHR DOT_CHR AND_CHR OR_CHR XOR_CHR BANG_CHR
%token TILDE_CHR ADD_CHR SUB_CHR STAR_CHR DIV_CHR MOD_CHR
%token OPEN_ANGLE_CHR CLOSE_ANGLE_CHR QUES_CHR
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INTEGER LONG SIGNED UNSIGNED FLOATING DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS EOF
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token ASM

%type <(Cparse.var_declaration list)> translation_unit

%start translation_unit
%%

primary_expression:
          identifier { 
                       (* DEBUG *) Debug.pRINT_DEBUG_PARSE "primary_expression : identifier";
                       let loc, var = $1 in
                       let qtyp, name, glob = var_type loc var in
			   match qtyp with
			       (_, TFUN _) ->
			       loc, ((true, false), TPOINTER qtyp), FUN name
			     | (_, TARRAY (_, _)) ->
			       loc, qtyp, ARRAY (name, glob)
			     | (_, TVOID) ->
			       (error (Some loc) ("cannot use void identifier " ^ var);
				loc, qtyp, VAR (name, glob))
			     | _ -> loc, qtyp, VAR (name, glob) }

        | constant { 
                      (* DEBUG *) Debug.pRINT_DEBUG_PARSE "primary_expression : constant";
                      let loc, typ, cst = $1 in loc, ((true, false), typ), CST cst }
        | string_literal { 
                       (* DEBUG *) Debug.pRINT_DEBUG_PARSE "primary_expression : string_literal";
                       let loc, s = $1 in loc, string_cst_type s, CST (STRING s) }
        | OPEN_PAREN_CHR expression CLOSE_PAREN_CHR { 
                       (* DEBUG *) Debug.pRINT_DEBUG_PARSE "primary_expression : expression";
                       $2 }
        ;

constant : CONSTANT { 
                       (* DEBUG *) Debug.pRINT_DEBUG_PARSE "constant : ";
                       let typ, cst = $1 in getloc (), typ, cst }

string_literal:
          STRING_LITERAL { 
                            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "string_literal : empty";
                            getloc (), $1 }
        | STRING_LITERAL string_literal 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "string_literal : string_literal";
              let (l,s) = $2 in
              let s2 = $1 in
              (getloc (), (s2^s))
            } 

inc_op : INC_OP { getloc () }
dec_op : DEC_OP { getloc () }

postfix_expression:
          primary_expression { 
                               (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : primary_expression";
                               $1 }
        | postfix_expression OPEN_BRACKET_CHR expression close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression .. expression close_bracket";
          mk_index (sup_locator (loc_of_expr $1) $4) $1 $3 }
        | postfix_expression OPEN_PAREN_CHR close_paren
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression .. close_paren";
          mk_apply (sup_locator (loc_of_expr $1) $3) $1 [] }
        | postfix_expression OPEN_PAREN_CHR argument_expression_list close_paren
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression .. argument_expression_list close_paren";
          mk_apply (sup_locator (loc_of_expr $1) $4) $1 (List.rev $3) }

        | postfix_expression DOT_CHR identifier
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression DOT_CHR identifier";
          let (loc, f) = $3 in 
          mk_sel (sup_locator (loc_of_expr $1) loc) $1 f }

        | postfix_expression PTR_OP identifier
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression PTR_OP identifier";
          let loc = sup_locator (loc_of_expr $1) (fst $3) in
          mk_sel loc (mk_ptr loc $1) (snd $3) }

        | postfix_expression inc_op
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression inc_op";
          mk_post_incdec (sup_locator (loc_of_expr $1) $2) true $1 }
        | postfix_expression dec_op
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "postfix_expression : postfix_expression dec_op";
          mk_post_incdec (sup_locator (loc_of_expr $1) $2) false $1 }
        ;

/* Les argument_expression_list sont des listes a l'envers */

argument_expression_list:
          assignment_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "argument_expression_list : assignment_expression";
          [$1] }
        | argument_expression_list COMMA_CHR assignment_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "argument_expression_list : argument_expression_list COMMA_CHR assignment_expression";
          $3 :: $1 }
        ;

sizeof : SIZEOF { getloc () };

unary_expression:
          postfix_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "unary_expression : postfix_expression";
          $1 }
        | inc_op unary_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "unary_expression :  inc_op unary_expression";
          mk_pre_incdec (sup_locator $1 (loc_of_expr $2)) true $2 }
        | dec_op unary_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "unary_expression : dec_op unary_expression";
          mk_pre_incdec (sup_locator $1 (loc_of_expr $2)) false $2 }
        | unary_operator cast_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "unary_expression : unary_operator cast_expression";
          let loc, c = $1 in
          let loc' = sup_locator loc (loc_of_expr $2) in
	  match c with
	      STAR_CHR -> mk_ptr loc' $2
	    | ADD_CHR -> (cast_to_number loc' (qtyp_of_expr $2);
			  $2)
	    | SUB_CHR -> mk_minus loc' $2
	    | BANG_CHR -> mk_bang loc' $2
            | TILDE_CHR -> mk_bnot loc' $2
            | AND_CHR -> mk_address_of loc' $2
	    | _ -> (Error.error (Some loc) "unknown unary operator";
		     loc, ((true, false), TINT), CST (INT 0.0)) }
        | sizeof unary_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "unary_expression : sizeof unary_expression";
          let loc = loc_of_expr $2 in
	  sup_locator $1 loc, ((true, false), TINT),
	  CST (INT (float_of_int (try sizeof_ctype (qtyp_of_expr $2)
				  with Sizeof -> (Error.error (Some loc)
					"cannot compute size of expression"; 0)))) }
        | sizeof OPEN_PAREN_CHR type_name close_paren
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "unary_expression : sizeof OPEN_PAREN_CHR type_name close_paren";
          let loc = sup_locator $1 $4 in
	  loc, ((true, false), TINT),
	  CST (INT (float_of_int (try sizeof_ctype $3
				  with Sizeof -> (Error.error (Some loc)
						"cannot compute size of type"; 0)))) }
        ;

unary_operator:
          star_chr  { $1 }
        | add_chr   { $1 }
        | sub_chr   { $1 }
        | bang_chr  { $1 }
        | tilde_chr { $1 }
        | and_chr   { $1 }
        ;

star_chr    : STAR_CHR  { getloc (), STAR_CHR  }
add_chr     : ADD_CHR   { getloc (), ADD_CHR   }
sub_chr     : SUB_CHR   { getloc (), SUB_CHR   }
bang_chr    : BANG_CHR  { getloc (), BANG_CHR  }
tilde_chr   : TILDE_CHR { getloc (), TILDE_CHR }
and_chr     : AND_CHR   { getloc (), AND_CHR   }

open_paren  : OPEN_PAREN_CHR  { getloc () }
close_paren : CLOSE_PAREN_CHR { getloc () }

cast_expression:
          unary_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "cast_expression : unary_expression";
          $1 }
        | open_paren type_name CLOSE_PAREN_CHR cast_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "cast_expression : open_paren type_name CLOSE_PAREN_CHR cast_expression";
          cast_to_type (sup_locator $1 (loc_of_expr $4)) $2 $4 }
        ;

multiplicative_expression:
          cast_expression { 
                            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "multiplicative_expression :  cast_expression";
                            $1 }
        | multiplicative_expression STAR_CHR cast_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "multiplicative_expression : multiplicative_expression STAR_CHR cast_expression";
          mk_setop S_MUL (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        | multiplicative_expression DIV_CHR cast_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "multiplicative_expression : multiplicative_expression DIV_CHR cast_expression";
          mk_setop S_DIV (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        | multiplicative_expression MOD_CHR cast_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "multiplicative_expression : multiplicative_expression MOD_CHR cast_expression";
          mk_setop S_MOD (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        ;

additive_expression:
          multiplicative_expression 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "additive_expression : multiplicative_expression";
              $1 }
        | additive_expression ADD_CHR multiplicative_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "additive_expression : additive_expression ADD_CHR multiplicative_expression";
          mk_setop S_ADD (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        | additive_expression SUB_CHR multiplicative_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "additive_expression : additive_expression SUB_CHR multiplicative_expression";
          mk_setop S_SUB (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        ;

shift_expression:
          additive_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "shift_expression : additive_expression";
          $1 }
        | shift_expression LEFT_OP additive_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "shift_expression : shift_expression LEFT_OP additive_expression";
          mk_setop S_LEFT (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        | shift_expression RIGHT_OP additive_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "shift_expression : shift_expression RIGHT_OP additive_expression";
          mk_setop S_RIGHT (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        ;

relational_expression:
          shift_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "relational_expression : shift_expression";
          $1 }
        | relational_expression OPEN_ANGLE_CHR shift_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "relational_expression : relational_expression OPEN_ANGLE_CHR shift_expression";
          mk_cmp C_LT (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	         $1 $3 }
        | relational_expression CLOSE_ANGLE_CHR shift_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "relational_expression : relational_expression CLOSE_ANGLE_CHR shift_expression";
          mk_cmp C_LT (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	         $3 $1 }
        | relational_expression LE_OP shift_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "relational_expression : relational_expression LE_OP shift_expression";
          mk_cmp C_LE (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	         $1 $3 }
        | relational_expression GE_OP shift_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "relational_expression : relational_expression GE_OP shift_expression";
          mk_cmp C_LE (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	         $3 $1 }
        ;

equality_expression:
          relational_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "equality_expression : relational_expression";
          $1 }
        | equality_expression EQ_OP relational_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "equality_expression : equality_expression EQ_OP relational_expression";
          mk_cmp C_EQ (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	         $1 $3 }
        | equality_expression NE_OP relational_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "equality_expression : equality_expression NE_OP relational_expression";
          let loc = sup_locator (loc_of_expr $1) (loc_of_expr $3) in
	  mk_bang loc (mk_cmp C_EQ loc $1 $3) }
        ;

and_expression:
          equality_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "and_expression : equality_expression";
          $1 }
        | and_expression AND_CHR equality_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "and_expression : and_expression AND_CHR equality_expression";
          mk_setop S_AND (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	           $1 $3 }
        ;

exclusive_or_expression:
          and_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "exclusive_or_expression : and_expression";
          $1 }
        | exclusive_or_expression XOR_CHR and_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "exclusive_or_expression : exclusive_or_expression XOR_CHR and_expression";
          mk_setop S_XOR (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	           $1 $3 }
        ;

inclusive_or_expression:
          exclusive_or_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "inclusive_or_expression : exclusive_or_expression";
          $1 }
        | inclusive_or_expression OR_CHR exclusive_or_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "inclusive_or_expression : inclusive_or_expression OR_CHR exclusive_or_expression";
          mk_setop S_OR (sup_locator (loc_of_expr $1) (loc_of_expr $3))
	           $1 $3 }
        ;

logical_and_expression:
          inclusive_or_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "logical_and_expression : inclusive_or_expression";
          $1 }
        | logical_and_expression AND_OP inclusive_or_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "logical_and_expression : logical_and_expression AND_OP inclusive_or_expression";
          mk_and (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        ;

logical_or_expression:
          logical_and_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "logical_or_expression : logical_and_expression";
          $1 }
        | logical_or_expression OR_OP logical_and_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "logical_or_expression : logical_or_expression OR_OP logical_and_expression";
          mk_or (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        ;

conditional_expression:
          logical_or_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "conditional_expression : logical_or_expression";
          $1 }
        | logical_or_expression QUES_CHR expression COLON_CHR conditional_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "conditional_expression : logical_or_expression QUES_CHR expression COLON_CHR conditional_expression";
          mk_if (sup_locator (loc_of_expr $1) (loc_of_expr $5)) $1 $3 $5 }
        ;

assignment_expression:
          conditional_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "assignment_expression : conditional_expression";
          $1 }
        | unary_expression assignment_operator assignment_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "assignment_expression : unary_expression assignment_operator assignment_expression";
          let loc = sup_locator (loc_of_expr $1) (loc_of_expr $3) in
	  mk_set_setop $2 loc $1 $3 }
        ;

assignment_operator:
          EQ_CHR       { S_EQ    }
        | MUL_ASSIGN   { S_MUL   }
        | DIV_ASSIGN   { S_DIV   }
        | MOD_ASSIGN   { S_MOD   }
        | ADD_ASSIGN   { S_ADD   }
        | SUB_ASSIGN   { S_SUB   }
        | LEFT_ASSIGN  { S_LEFT  }
        | RIGHT_ASSIGN { S_RIGHT }
        | AND_ASSIGN   { S_AND   }
        | XOR_ASSIGN   { S_XOR   }
        | OR_ASSIGN    { S_OR    }
        ;

expression:
          assignment_expression { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "expression : assignment_expression";
          $1 }
        | expression COMMA_CHR assignment_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "expression : expression COMMA_CHR assignment_expression";
          mk_eseq (sup_locator (loc_of_expr $1) (loc_of_expr $3)) $1 $3 }
        ;

constant_expression:
          conditional_expression
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "constant_expression : conditional_expression";
          loc_of_expr $1, try
	   convert (qtyp_of_expr $1) (eval_constant_loc_expr $1)
	  with WrongType -> (error (Some (loc_of_expr $1)) "wrong type in constant expression";
				((true, false), TINT), INT 0.0)
            | NotConstantExpression _ ->
	      (Error.error (Some (loc_of_expr $1))
	       (let buf = Buffer.create 128 in
	        (bufout_loc_expr buf 0 $1;
		 Buffer.add_string buf " is not constant";
		 Buffer.contents buf));
	       ((true, false), TINT), INT 0.0)
        }
        ;
/* FABRICE : ajouter les const.. */

declaration
	: TYPEDEF type_specifier init_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : TYPEDEF type_specifier init_declarator_list SEMI_CHR";
          List.iter (fun (loc, var, transform) ->
			let buf = Buffer.create 128 in
			let qtyp = transform $2 in
			(output_string types_file "\ntypedef ";
			 bufout_type_name buf qtyp var;
			 Buffer.output_buffer types_file buf;
			 output_string types_file ";\n";
			 Hashtbl.add typedefs var qtyp)) $3;
          []
	}

        /* Fabrice : ici avec le lex correspondant, c'est forcement une erreur. */
        | TYPEDEF type_specifier TYPE_NAME SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : TYPEDEF type_specifier TYPE_NAME SEMI_CHR";
          (fatal (Some (getloc ()))
	     ("type `" ^ $3 ^ "' previously declared"));
          []
	}


	| optional_type_qualifier_list type_specifier optional_init_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : optional_type_qualifier_list type_specifier optional_init_declarator_list SEMI_CHR";
          List.map (fun (loc, var, transform) ->
			declare_var CQ_DEFAULT loc var
	                (transform (mk_type $1 $2))) $3 }

        | register_or_auto optional_type_qualifier_list type_specifier optional_init_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : register_or_auto optional_type_qualifier_list type_specifier optional_init_declarator_list SEMI_CHR";
          List.map (fun (loc, var, transform) ->
			declare_var CQ_DEFAULT loc var
	                (transform (mk_type $2 $3))) $4 }
        
        | enum_specifier SEMI_CHR  
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : enum_specifier SEMI_CHR";
              resetenums (); [] } 

	| EXTERN optional_type_qualifier_list type_specifier init_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : EXTERN optional_type_qualifier_list type_specifier init_declarator_list SEMI_CHR";
          List.map (fun (loc, var, transform) ->
			declare_var CQ_EXTERN loc var
			(transform (mk_type $2 $3))) $4 }

	| STATIC optional_type_qualifier_list type_specifier init_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : STATIC optional_type_qualifier_list type_specifier init_declarator_list SEMI_CHR";
          List.map (fun (loc, var, transform) ->
	    declare_var CQ_STATIC loc var
	      (transform (mk_type $2 $3))) $4
        }

        /*                                                                      */
        /* FABRICE : on ajoute le CONST STATIC.                                 */
        /* Principalement, on le traite comme un static, mais on force le const */
        /* ....                                                                 */
        /*                                                                      */

        | CONST STATIC optional_type_qualifier_list type_specifier init_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration : CONST  STATIC optional_type_qualifier_list type_specifier init_declarator_list SEMI_CHR";
          List.map (fun (loc, var, transform) ->
			declare_var CQ_STATIC loc var
			(transform (mk_type (true,snd($3)) $4))) $5 }
        ;

/*obsolete
declaration_specifiers
        : storage_class_specifier
        | storage_class_specifier declaration_specifiers
        | type_specifier
        | type_specifier declaration_specifiers
        | type_qualifier
        | type_qualifier declaration_specifiers
        ;
*/

optional_init_declarator_list : 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "optional_init_declarator_list : Empty";
            [] }
	| init_declarator_list { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "optional_init_declarator_list : init_declarator_list";
            $1 }
	;

/* Une init_declarator_list est une liste a l'envers de declarator. */
init_declarator_list
        : init_declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "init_declarator_list : init_declarator";
              [$1] }
        | init_declarator_list COMMA_CHR init_declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "init_declarator_list : init_declarator_list COMMA_CHR init_declarator";
              $3 :: $1 }
        ;

init_declarator:
          declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "init_declarator : declarator ";
              $1 }

        | declarator EQ_CHR iNitializer
        {
         
         (* DEBUG *) Debug.pRINT_DEBUG_PARSE "init_declarator : declarator EQ_CHR iNitializer";

         (* Ici, nous avons besoin de définir ce qu'est un iNinitalizer. *)
         let decl = $1 in
         let init = $3 in
         
         (* print_string "\n On passe dans un init"; flush stdout; *)

         (** Ici init est une liste d'assignement expressions. **)

         (* FABRICE : A FAIRE  : TBD *)
         $1
       } 
        ;

/*obsolete
storage_class_specifier
        : TYPEDEF { () }
        | EXTERN
        | STATIC
        | AUTO { () }
        | REGISTER { () }
        ;
*/

register_or_auto:
          REGISTER                  { () }
        | AUTO                      { () }
        | REGISTER register_or_auto { () }
        | AUTO     register_or_auto { () }
      ;

/* Fabrice, on surcharge la definition des types, au cas ou un const
            est placé devant une définition de types.                 */

type_specifier:
            type_specifier_nc  { $1 }
        |   CONST type_specifier_nc 
            { let ((_,b),t) = $2 in
              ((true,b),t) }
      ;


type_specifier_nc:
          VOID                   { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: void";
          (false, false), TVOID }

        | VOID CONST              { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: void const";
          (true, false), TVOID }

        | CHAR                   {
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: char";
          (false, false), TCHAR }

        | CHAR CONST            {
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: char const";
          (true, false), TCHAR }

        | SIGNED CHAR            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed char";
          (false, false), TCHAR }

        | SIGNED CHAR CONST    {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed char const";
          (true, false), TCHAR }

        | SIGNED CONST CHAR            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed const char";
          (true, false), TCHAR }

	| UNSIGNED CHAR          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned char";
          (false, false), TUNSIGNED_CHAR }

	| UNSIGNED CHAR CONST    {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned char const";
          (true, false), TUNSIGNED_CHAR }

	| UNSIGNED CONST CHAR          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned const char";
          (true, false), TUNSIGNED_CHAR }

        | SHORT                  {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: short";
          (false, false), TSHORT }

        | SHORT CONST            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: short";
          (true, false), TSHORT }

	| SHORT INTEGER          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: short integer";
          (false, false), TSHORT }

	| SHORT INTEGER CONST         {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: short integer";
          (true, false), TSHORT }

	| SHORT CONST INTEGER          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: short const integer";
          (true, false), TSHORT }

        | SIGNED SHORT           {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed short";
          (false, false), TSHORT }

        | SIGNED SHORT CONST          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed short";
          (true, false), TSHORT }

        | SIGNED CONST SHORT           {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed const short";
          (true, false), TSHORT }

	| SIGNED SHORT INTEGER   {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed short integer";
          (false, false), TSHORT }

	| SIGNED CONST SHORT INTEGER   {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed const short integer";
          (true, false), TSHORT }

	| SIGNED  SHORT CONST INTEGER   {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed short const integer";
          (true, false), TSHORT }

	| UNSIGNED SHORT         {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsgned short";
          (false, false), TUNSIGNED_SHORT }

	| UNSIGNED SHORT CONST        {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsgned short";
          (true, false), TUNSIGNED_SHORT }

	| UNSIGNED CONST SHORT         {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsgned const short";
          (true, false), TUNSIGNED_SHORT }

	| UNSIGNED SHORT INTEGER {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned short integer";
          (false, false), TUNSIGNED_SHORT }

	| UNSIGNED CONST SHORT INTEGER {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned short integer";
          (true, false), TUNSIGNED_SHORT }

	| UNSIGNED SHORT CONST INTEGER {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned short integer";
          (true, false), TUNSIGNED_SHORT }

        | INTEGER                {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: integer";
          (false, false), TINT }

        | INTEGER CONST               {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: integer";
          (true, false), TINT }

        | SIGNED INTEGER         {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed integer";
          (false, false), TINT }

        | SIGNED CONST INTEGER         {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed integer";
          (true, false), TINT }

	| UNSIGNED INTEGER       {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned integer";
          (false, false), TUNSIGNED_INT }

	| UNSIGNED CONST INTEGER       {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned integer";
          (true, false), TUNSIGNED_INT }

        | UNSIGNED               {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned";
          (false, false), TUNSIGNED_INT }

        | UNSIGNED CONST              {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned";
          (true, false), TUNSIGNED_INT }

        | LONG                   {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long";
          (false, false), TLONG }

        | LONG CONST              {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long";
          (true, false), TLONG }

        | SIGNED LONG            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed long";
          (false, false), TLONG }

        | SIGNED CONST LONG            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed long";
          (true, false), TLONG }

        | LONG INTEGER           {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long integer";
          (false, false), TLONG }

        | LONG CONST INTEGER           {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long integer";
          (true, false), TLONG }

        | SIGNED LONG INTEGER    {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed long integer";
          (false, false), TLONG }

        | SIGNED CONST LONG INTEGER    {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed long integer";
          (true, false), TLONG }

        | SIGNED LONG CONST INTEGER    {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: signed long integer";
          (true, false), TLONG }

        | UNSIGNED LONG          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long";
          (false, false), TUNSIGNED_LONG }

        | UNSIGNED CONST LONG          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long";
          (true, false), TUNSIGNED_LONG }

        | UNSIGNED LONG INTEGER  {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long integer";
          (false, false), TUNSIGNED_LONG }

        | UNSIGNED CONST LONG INTEGER  {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long integer";
          (true, false), TUNSIGNED_LONG }

        | UNSIGNED LONG CONST INTEGER  {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long integer";
          (true, false), TUNSIGNED_LONG }

        | FLOATING               {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: floating";
          (false, false), TFLOAT }

        | FLOATING CONST         {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: floating";
          (true, false), TFLOAT }

        | DOUBLE                 {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: double";
          (false, false), TDOUBLE }

        | DOUBLE CONST                {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: double";
          (true, false), TDOUBLE }

        | LONG LONG INTEGER      {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long long integer";
          (false, false), TDOUBLE }

        | LONG LONG       {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long long ";
          (false, false), TDOUBLE }

        | UNSIGNED LONG LONG INTEGER {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long long integer";
          (false, false), TDOUBLE }
        | UNSIGNED LONG LONG {
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: unsigned long long";
          (false, false), TDOUBLE }

        | LONG DOUBLE            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long double";
          (false,false), TLONG_DOUBLE}
            
        | LONG CONST DOUBLE            {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier: long double";
          (true,false), TLONG_DOUBLE}

        | struct_or_union_specifier {
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier : struct_or_union_specifier ";
          (false, false), $1 }

        /* On définit un type enuméré. */
        | enum_specifier         
            {
             
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier : enum_specifier";
             
             resetenums ();
             ((false, false), TINT)
           } 

        | TYPE_NAME { 
          
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_specifier : TYPE_NAME";

          try Hashtbl.find typedefs $1
		      with Not_found -> (error (Some (getloc ()))
					 ("undefined type name " ^ $1);
					 (false, false), TSTRUCT $1) }
        ;


identifier  : IDENTIFIER      { getloc (), $1 };
open_brace  : OPEN_BRACE_CHR  { getloc () };
close_brace : CLOSE_BRACE_CHR { getloc () };

/* Fabrice ET JEAN : on ajoute ceci, a cause des inbrications TYPEDEF / STRUCT */

identifier_or_type_name : 
            identifier  { $1 }
        |   TYPE_NAME   { getloc (), $1 }

struct_or_union_specifier:
           struct_or_union identifier_or_type_name OPEN_BRACE_CHR checked_struct_declaration_list CLOSE_BRACE_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_or_union_specifier : struct_or_union identifier OPEN_BRACE_CHR checked_struct_declaration_list CLOSE_BRACE_CHR";
          $1 (fst $2) (snd $2) (Some $4) }
        | struct_or_union open_brace checked_struct_declaration_list CLOSE_BRACE_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_or_union_specifier : struct_or_union open_brace checked_struct_declaration_list CLOSE_BRACE_CHR";
          $1 $2 (gensym "_struct") (Some $3) }
        | struct_or_union identifier_or_type_name
	{ 
          let name = (snd $2) in
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE ("struct_or_union_specifier : struct_or_union identifier "^(name));
          $1 (fst $2) (snd $2) None }
        ;

struct_or_union
        : STRUCT { 
                 (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_or_union : STRUCT";
                 fun loc -> fun name -> fun fields ->
		 (output_string types_file "\nstruct ";
		  output_string types_file name;
		  output_string types_file " ";
		  (let buf = Buffer.create 128 in
		   (bufout_fields buf fields;
		    Buffer.output_buffer types_file buf));
		  output_string types_file ";\n";
		  (match fields with
			Some flds -> (try
					let _ = Hashtbl.find structs name in
					Error.error (Some loc)
					  ("struct " ^ name ^ " is already defined")
				      with Not_found ->
					 Hashtbl.add structs name flds)
		      | None -> ());
		      TSTRUCT name) }
        | UNION { 
                (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_or_union : UNION";
                fun loc -> fun name -> fun fields ->
		(output_string types_file "\nunion ";
		 output_string types_file name;
		 output_string types_file " ";
		 (let buf = Buffer.create 128 in
		  (bufout_fields buf fields;
		   Buffer.output_buffer types_file buf));
		 output_string types_file ";\n";
		 (match fields with
			Some flds -> (try
					let _ = Hashtbl.find unions name in
					Error.error (Some loc)
					  ("union " ^ name ^ " is already defined")
				      with Not_found -> Hashtbl.add unions name flds)
		      | None -> ());
		      TUNION name) }
        ;

checked_struct_declaration_list: 
            struct_declaration_list
	    { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "checked_struct_declaration_list : struct_declaration_list";
              let h = Hashtbl.create 48 in
	    (List.iter (fun (loc, var, _) ->
	      try
		let _ = Hashtbl.find h var in
		Error.error (Some loc) ("duplicate field name " ^ var)
	      with Not_found -> Hashtbl.add h var loc) $1;
	     $1) };

struct_declaration_list
        : struct_declaration 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declaration_list : struct_declaration";
              $1 }
        | struct_declaration_list struct_declaration 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declaration_list : struct_declaration_list struct_declaration";
              $1 @ $2 }
        ;

struct_declaration
/*      : specifier_qualifier_list struct_declarator_list SEMI_CHR */
	: optional_type_qualifier_list type_specifier struct_declarator_list SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declaration : optional_type_qualifier_list type_specifier struct_declarator_list SEMI_CHR";
          List.map (fun (loc, var, transform) ->
                    loc, var, transform (mk_type $1 $2)) $3 }
        | optional_type_qualifier_list type_specifier struct_declarator_list COLON_CHR CONSTANT SEMI_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declaration : optional_type_qualifier_list type_specifier struct_declarator_list COLON_CHR CONSTANT SEMI_CHR";
          List.map (fun (loc, var, transform) ->
                    loc, var, transform (mk_type $1 $2)) $3 }
            
        /* FABRICE, Attention, ici gestion des cas "unsigned int:16;" comme le cas vide. */
        /* FABRICE, mais il faut aussi traiter le cas des instructions du style          */
        /* unsigned int ttt1:4, ttt2:4; */
        | optional_type_qualifier_list type_specifier  COLON_CHR CONSTANT SEMI_CHR
        { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declaration : optional_type_qualifier_list type_specifier  COLON_CHR CONSTANT SEMI_CHR";
          [] }
        ;

/*obsolete
specifier_qualifier_list
        : type_specifier specifier_qualifier_list
        | type_specifier
        | type_qualifier specifier_qualifier_list
        | type_qualifier
        ;
*/

/* les struct_declarator_list sont des listes inversees */
struct_declarator_list
        : struct_declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declarator_list : struct_declarator";
              [$1] }
        | struct_declarator_list COMMA_CHR struct_declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declarator_list : struct_declarator_list COMMA_CHR struct_declarator";
              $3 :: $1 }
        ;

struct_declarator: 
         declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declarator : declarator";
              $1 }
        /* FABRICE, mais il faut aussi traiter le cas des instructions du style  */
        /* unsigned int ttt1:4, ttt2:4;                                          */
        /* Cas du bitfields : ignoré.                                            */

        | declarator COLON_CHR CONSTANT 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "struct_declarator : declarator COLON_CHR CONSTANT";
              $1 } 
/*
        | COLON_CHR constant_expression
        | declarator COLON_CHR constant_expression
        ???
*/
        ;

/* Definition des types enumeres. */

enum_specifier
        : ENUM OPEN_BRACE_CHR enumerator_list CLOSE_BRACE_CHR             
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enum_specifier : ENUM OPEN_BRACE_CHR enumerator_list CLOSE_BRACE_CHR";
              () }
        | ENUM IDENTIFIER OPEN_BRACE_CHR enumerator_list CLOSE_BRACE_CHR  
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enum_specifier : ENUM IDENTIFIER OPEN_BRACE_CHR enumerator_list CLOSE_BRACE_CHR";
              () }
        | ENUM IDENTIFIER  
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enum_specifier : ENUM IDENTIFIER";
              () }
        ;

/* Un enumerator_list est une liste ordonnee de définition d'enumeration. */
enumerator_list:
          enumerator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enumerator_list : enumerator";
              () }
        | enumerator_list COMMA_CHR enumerator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enumerator_list : enumerator_list COMMA_CHR enumerator";
              () }
         ;


/* Un enumerator est un identificateur avec possiblement une valeur d'init (type optionnel). */
enumerator:
          IDENTIFIER  
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enumerator : IDENTIFIER";
              enum_add_hash ($1,None)
          }
        | IDENTIFIER EQ_CHR constant_expression
            {
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE "enumerator : IDENTIFIER EQ_CHR";
             let (_,(_,ce)) = $3 in
             match ce with
               INT n -> enum_add_hash ($1, (Some n))
             | _ -> parse_error ("enumerator: invalid enum initialisation")
           } 
        ;

/* */

type_qualifier
        : CONST           { true, false }
        | CONST REGISTER  { true, false } /* FABRICE : TBD Je l'ai vu dans le code de SSL, on ignore le register !!. */
        | VOLATILE        { false, true }
        ;

/* un declarateur est un "usage typique" d'un nom de variable ou de champ.
	Par exemple, ( *a )[20] est un pointeur sur un tableau de 20 quelquechose.
   On convertit ca ici en un couple donnant le nom a de la variable ou du champ,
   plus une fonction qui prend en entree le type de l "usage typique" (ex: int)
   et sort le type de a (avec l'exemple ci-dessus, pointeur sur tableau de 20 int).
*/
declarator :
         pointer declarator
	 { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declarator : pointer declarator";
          
          let loc, var, transform = $2 in
	  loc, var, fun typ -> transform ($1, TPOINTER typ) }
        | direct_declarator 
          { 
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declarator : direct_declarator";
           $1 }
        ;

open_bracket : OPEN_BRACKET_CHR { getloc () }
close_bracket : CLOSE_BRACKET_CHR { getloc () }

direct_declarator
        : identifier 
         { 
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : identifier";
           let loc, var = $1 in loc, var, fun typ -> typ }

        | OPEN_PAREN_CHR declarator CLOSE_PAREN_CHR 
          { 
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : OPEN_PAREN_CHR declarator CLOSE_PAREN_CHR";
           $2 }
            
        | direct_declarator open_bracket constant_expression close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : direct_declarator open_bracket constant_expression close_bracket";
          let loc, var, transform = $1 in
	  let (loc', (qtyp, cst)) = $3 in
	  match cst with
	    INT n -> loc, var, fun typ -> ((false, false), TARRAY (transform typ,
								   Some (int_of_float n)))
	  | _ -> (Error.error (Some (sup_locator $2 $4)) "dimension should be constant";
		  loc, var, fun typ -> ((false, false), TARRAY (transform typ, None)))
	}

        | direct_declarator open_bracket close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : direct_declarator open_bracket close_bracket";
          let loc, var, transform = $1 in
	  loc, var, fun typ -> ((false, false), TARRAY (transform typ, None))
	}

        | direct_declarator OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : direct_declarator OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR";
          let loc, var, transform = $1 in
	  loc, var, fun typ -> ((false, false), TFUN (T_PROTOTYPE (fst $3, snd $3), transform typ))
	}

        |  direct_declarator OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR  ASM  OPEN_PAREN_CHR STRING_LITERAL CLOSE_PAREN_CHR
            {
(* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR  ASM  OPEN_PAREN_CHR STRING_LITERAL CLOSE_PAREN_CHR";
             let loc, var, transform = $1 in
	     loc, var, fun typ -> ((false, false), TFUN (T_PROTOTYPE (fst $3, snd $3), transform typ))
           } 

/*      | direct_declarator OPEN_PAREN_CHR identifier_list CLOSE_PAREN_CHR */

        | direct_declarator OPEN_PAREN_CHR CLOSE_PAREN_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_declarator : direct_declarator OPEN_PAREN_CHR CLOSE_PAREN_CHR";
          let loc, var, transform = $1 in
	  loc, var, fun typ -> ((false, false), TFUN (T_PROTOTYPE ([], true), transform typ)) }
        ;

pointer :
        STAR_CHR optional_type_qualifier_list 
        { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "pointer : STAR_CHR optional_type_qualifier_list";
          $2 }
        ;

optional_type_qualifier_list: 
          type_qualifier_list 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "optional_type_qualifier_list : type_qualifier_list";
            $1 }
	| { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "optional_type_qualifier_list : other";
            false, false }
	;

type_qualifier_list
        : type_qualifier 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_qualifier_list : type_qualifier";
            $1 }
        | type_qualifier_list type_qualifier 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_qualifier_list : type_qualifier_list type_qualifier";
            (fst $1 || fst $2), (snd $1 || snd $2) }
        ;


parameter_type_list
        : parameter_list { 
                           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_type_list : parameter_list";
                           let l = List.rev $1 in
			   (match l with
			        [_, _, (_, TVOID)] -> []
			      | _ -> l), false}
        | parameter_list COMMA_CHR ELLIPSIS
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_type_list : parameter_list COMMA_CHR ELLIPSIS";
          List.rev $1, true }
        ;

/*!!!should check no repeated param name! */
/* Une parameter_list est une liste inversee de parameter_list. */
parameter_list
        : modified_parameter_declaration 
          { 
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_list : modified_parameter_declaration";
           [$1] }
        | parameter_list COMMA_CHR modified_parameter_declaration
          {
	   (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_list : parameter_list COMMA_CHR modified_parameter_declaration";
           $3 :: $1 }
        ;

modified_parameter_declaration : 
    parameter_declaration {
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "modified_parameter_declaration : parameter_declaration";
  	  (* Ici, si on prend un parametre de type T[] en entree, on le
	  transforme silencieusement en const T* *)
	  let loc, var, (((_, vol), typ) as qtyp) = $1 in
          let qtyp' = match typ with
		          TARRAY (qtyp, _) -> (true, vol), TPOINTER qtyp
			| _ -> qtyp in
 	      (loc, var, qtyp') }
        ;

parameter_declaration:
        optional_type_qualifier_list type_specifier declarator
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_declaration : optional_type_qualifier_list type_specifier declarator";
          let loc, var, transform = $3 in loc, var, transform (mk_type $1 $2) }

        | register_or_auto optional_type_qualifier_list type_specifier declarator
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_declaration : register_or_auto optional_type_qualifier_list type_specifier declarator";
          let loc, var, transform = $4 in loc, var, transform (mk_type $2 $3) }
        | optional_type_qualifier_list type_specifier
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_declaration : optional_type_qualifier_list type_specifier";
          let loc = getloc () in
	  let var = "" in
		loc, var, mk_type $1 $2 }
        | optional_type_qualifier_list type_specifier abstract_declarator
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "parameter_declaration : optional_type_qualifier_list type_specifier abstract_declarator";
          let loc = getloc () in
	  let var = "" in
	  let transform = $3 in
		loc, var, transform (mk_type $1 $2) }
/*        | declaration_specifiers abstract_declarator	*/
/*        | declaration_specifiers	*/
        ;

/*obsolete
identifier_list
        : IDENTIFIER
        | identifier_list COMMA_CHR IDENTIFIER
        ;
*/

type_name
	: optional_type_qualifier_list type_specifier 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_name : optional_type_qualifier_list type_specifier";
            mk_type $1 $2 }
	| optional_type_qualifier_list type_specifier abstract_declarator
	  { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "type_name : optional_type_qualifier_list type_specifier abstract_declarator";
            $3 (mk_type $1 $2) }
        ;

abstract_declarator: 
          pointer 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "abstract_declarator : pointer";
            fun qtyp -> $1, TPOINTER qtyp }
        | direct_abstract_declarator 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "abstract_declarator : direct_abstract_declarator";
            $1 }
        | pointer abstract_declarator 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "abstract_declarator : pointer abstract_declarator";
            fun qtyp -> $2 ($1, TPOINTER qtyp) }
        ;

direct_abstract_declarator: 
          OPEN_PAREN_CHR abstract_declarator CLOSE_PAREN_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator :  OPEN_PAREN_CHR abstract_declarator CLOSE_PAREN_CHR";
              $2 }
        | open_bracket close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : open_bracket close_bracket";
          fun qtyp -> ((false, false), TARRAY (qtyp, None)) }
        | open_bracket constant_expression close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : open_bracket constant_expression close_bracket";
          let loc', (qtyp, cst) = $2 in
	  match cst with
	    INT n -> fun qtyp -> ((false, false), TARRAY (qtyp, Some (int_of_float n)))
	  | _ -> (Error.error (Some (sup_locator $1 $3)) "dimension should be constant";
		  fun qtyp -> ((false, false), TARRAY (qtyp, None)))
	}
        | direct_abstract_declarator open_bracket close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : direct_abstract_declarator open_bracket close_bracket";
          fun qtyp -> ((false, false), TARRAY ($1 qtyp, None)) }
        | direct_abstract_declarator open_bracket constant_expression close_bracket
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : direct_abstract_declarator open_bracket constant_expression close_bracket";
          let loc', (qtyp, cst) = $3 in
	  match cst with
	    INT n -> fun qtyp -> ((false, false), TARRAY ($1 qtyp, Some (int_of_float n)))
	  | _ -> (Error.error (Some (sup_locator $2 $4)) "dimension should be constant";
		  fun qtyp -> ((false, false), TARRAY ($1 qtyp, None)))
	}
        | OPEN_PAREN_CHR CLOSE_PAREN_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : OPEN_PAREN_CHR CLOSE_PAREN_CHR";
          fun qtyp -> ((false, false), TFUN (T_PROTOTYPE ([], true), qtyp)) }
        | OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR";
          fun qtyp -> ((false, false), TFUN (T_PROTOTYPE (fst $2, snd $2), qtyp)) }
        | direct_abstract_declarator OPEN_PAREN_CHR CLOSE_PAREN_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : direct_abstract_declarator OPEN_PAREN_CHR CLOSE_PAREN_CHR";
          fun qtyp -> ((false, false), TFUN (T_PROTOTYPE ([], true), $1 qtyp)) }
        | direct_abstract_declarator OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "direct_abstract_declarator : direct_abstract_declarator OPEN_PAREN_CHR parameter_type_list CLOSE_PAREN_CHR";
          fun qtyp -> ((false, false), TFUN (T_PROTOTYPE (fst $3, snd $3), $1 qtyp)) }
        ;

/* Un initializer est une liste d'expression. */

iNitializer:
          assignment_expression
          { 
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iNitializer : assignment_expression";
           [$1] }
        | OPEN_BRACE_CHR iNitializer_list CLOSE_BRACE_CHR
          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iNitializer : OPEN_BRACE_CHR iNitializer_list CLOSE_BRACE_CHR";
           ($2)
          } 
        | OPEN_BRACE_CHR iNitializer_list COMMA_CHR CLOSE_BRACE_CHR
          {
           (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iNitializer : OPEN_BRACE_CHR iNitializer_list COMMA_CHR CLOSE_BRACE_CHR";
           ($2)
         } 
        ;

iNitializer_list:
          iNitializer
            {
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iNitializer_list : iNitializer";
             ($1)
           } 
        | iNitializer_list COMMA_CHR iNitializer
            {
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iNitializer_list : iNitializer_list COMMA_CHR iNitializer";
             ($3@$1)
           } 
        ;


statement
        : labeled_statement    
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : labeled_statement";
              $1 }
        | compound_statement   
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : compound_statement";
              $1 }
        | expression_statement 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : expression_statement";
              loc_of_expr $1, CEXPR $1 }
        | selection_statement  
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : selection_statement";
              $1 }
        | iteration_statement  
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : iteration_statement";
              $1 }
        | jump_statement       
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : jump_statement";
              $1 }

/* FABRICE : A VOIR
        | asm_statement
            {
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement : ASM Statement";
             $1
           } 
        ;
*/
/* les declarations ASM. */
/*
asm_statement:
         ASM  OPEN_PAREN_CHR STRING_LITERAL CLOSE_PAREN_CHR
            {
             let s = $3 in
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE ("asm_statement: "^s);
             failwith "ASM";
           } 
            ;
*/

case: CASE { getloc () };
default: DEFAULT { getloc () };

labeled_statement: 
        identifier COLON_CHR statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "labeled_statement : identifier COLON_CHR statement";
          sup_locator (fst $1) (fst $3), CLABEL (snd $1, $3) }
        | case constant_expression COLON_CHR statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "labeled_statement : case constant_expression COLON_CHR statement";
          let loc, c = $2 in
	  let (_, e) = convert ((true, false), TINT) c in
	  sup_locator $1 (fst $4), CCASE (sup_locator $1 loc,
					  e, $4) }
        | default COLON_CHR statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "labeled_statement : default COLON_CHR statement";
          sup_locator $1 (fst $3), CDEFAULT ($3) }
        ;

open_block : open_brace { push_types_stack (); $1 };
close_block : close_brace { pop_types_stack (); $1 };

compound_statement: 
          open_block close_block 
        { 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "compound_statement : open_block close_block";
          sup_locator $1 $2, CBLOCK ([], []) }
        | open_block statement_list close_block
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "compound_statement : open_block statement_list close_block";
          sup_locator $1 $3, CBLOCK ([], List.rev $2) }
        | open_block declaration_list close_block
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "compound_statement : open_block declaration_list close_block";
          sup_locator $1 $3, CBLOCK ($2, []) }
        | open_block declaration_list statement_list close_block
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "compound_statement : open_block declaration_list statement_list close_block"; 
          sup_locator $1 $4, CBLOCK ($2, List.rev $3) }
        ;

/* Une declaration_list est une liste non inversee de declaration */
declaration_list
        : declaration 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration_list : declaration";
            $1 }
        | declaration_list declaration 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "declaration_list : declaration_list declaration";
            $1 @ $2 }
        ;

/* Une statement_list est une liste inversee de statement */
statement_list
        : statement 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement_list : statement";
            [$1] }
        | statement_list statement 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "statement_list : statement_list statement";
            $2 :: $1 }
        ;

expression_statement: 
         semi_chr 
            { 
             (* DEBUG *) Debug.pRINT_DEBUG_PARSE "expression_statement : semi_chr";
             $1, ((true, false), TVOID), ESEQ [] }
        | expression SEMI_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "expression_statement : expression SEMI_CHR";
              $1 }
        ;

semi_chr : SEMI_CHR { getloc () }

ifkw : IF { getloc () };
switch : SWITCH { getloc () };

selection_statement
        : ifkw OPEN_PAREN_CHR expression CLOSE_PAREN_CHR statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "selection_statement : ifkw OPEN_PAREN_CHR expression CLOSE_PAREN_CHR statement";
          sup_locator $1 (fst $5), CIF (convert_to_int $3, $5,
					(getloc (), CBLOCK ([], []))) }
        | ifkw OPEN_PAREN_CHR expression CLOSE_PAREN_CHR statement ELSE statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "selection_statement : ifkw OPEN_PAREN_CHR expression CLOSE_PAREN_CHR statement ELSE statement";
          sup_locator $1 (fst $7), CIF (convert_to_int $3, $5, $7) }
        | switch OPEN_PAREN_CHR expression CLOSE_PAREN_CHR statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "selection_statement : switch OPEN_PAREN_CHR expression CLOSE_PAREN_CHR statement";
          sup_locator $1 (fst $5), CSWITCH (convert_to_int $3, $5) }
        ;

whilekw : WHILE { getloc () };
dokw : DO { getloc () };
forkw : FOR { getloc () };

iteration_statement
        : whilekw OPEN_PAREN_CHR expression close_paren statement
	/* while (e) c == loop { ; / if (e) c else break; } */
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iteration_statement : whilekw OPEN_PAREN_CHR expression close_paren statement";
          let loc = sup_locator $1 (fst $5) in
	  loc, CLOOP (($1, CBLOCK ([], [])),
		      (loc, CIF (convert_to_int $3, $5, ($4, CBREAK)))) }
        | dokw statement whilekw OPEN_PAREN_CHR expression close_paren semi_chr
	/* do c while (e) == loop { if (e) ; else break; / c } */
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iteration_statement : dokw statement whilekw OPEN_PAREN_CHR expression close_paren semi_chr";
          let loc = sup_locator $1 $6 in
	  loc, CLOOP ((loc, CIF (convert_to_int $5, ($7, CBLOCK ([], [])),
				 ($7, CBREAK))),
		      $2) }
        | forkw OPEN_PAREN_CHR expression_statement expression_statement close_paren statement
	/* for (e0; e; ) c == e0; loop { ; / if (e) c else break; } */
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iteration_statement : forkw OPEN_PAREN_CHR expression_statement expression_statement close_paren statement";
          let loc = sup_locator $1 (fst $6) in
	  loc, CBLOCK ([], [(loc_of_expr $3, CEXPR $3);
	  loc, CLOOP ((getloc
 (), CBLOCK ([], [])),
		      (loc, CIF (convert_to_int $4, $6, ($5, CBREAK))))]) }
        | forkw OPEN_PAREN_CHR expression_statement expression_statement expression close_paren statement
	/* for (e0; e; e1) c == e0; loop { e1; / if (e) c else break; } */
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "iteration_statement : forkw OPEN_PAREN_CHR expression_statement expression_statement expression close_paren statement";
          let loc = sup_locator $1 (fst $7) in
	  loc, CBLOCK ([], [(loc_of_expr $3, CEXPR $3);
	  loc, CLOOP ((loc_of_expr $5, CEXPR $5),
		      (loc, CIF (convert_to_int $4, $7, ($6, CBREAK))))]) }
        ;

goto : GOTO { getloc () };
continue : CONTINUE { getloc () };
break : BREAK { getloc () };
return : RETURN { getloc () };

jump_statement
        : goto identifier SEMI_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "jump_statement : goto identifier SEMI_CHR";
              sup_locator $1 (fst $2), CGOTO (snd $2) }
        | continue SEMI_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "jump_statement : continue SEMI_CHR";
              $1, CCONTINUE }
        | break SEMI_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "jump_statement : break SEMI_CHR";
              $1, CBREAK }
/*!!! Verifier type de retour dans return; idem s'il n'y a pas de return */
        | return SEMI_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "jump_statement : return SEMI_CHR";
              $1, CRETURN None }
        | return expression SEMI_CHR 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "jump_statement : return expression SEMI_CHR";
              sup_locator $1 (loc_of_expr $2), CRETURN (Some $2) }
        ;

translation_unit:
          external_declaration 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "translation_unit : external_declaration";
            $1 }
        | translation_unit external_declaration 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "translation_unit : translation_unit external_declaration";
            $1 @ $2 }
        | EOF 
          { 
            (* DEBUG *) Debug.pRINT_DEBUG_PARSE "translation_unit : EOF";
            [] } /* Fabrice, on veux lire des fichiers vides. */
        ;

external_declaration
        : function_definition 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "external_declaration : function_definition";
              [$1] }
        | declaration 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "external_declaration : declaration";
              $1 }
        ;

function_declarator : optional_type_qualifier_list type_specifier declarator
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "function_declarator : optional_type_qualifier_list type_specifier declarator";
          let loc, var, transform = $3 in
	  let typ = transform (mk_type $1 $2) in
	  ((match typ with
	        (_, TFUN (params, res)) -> (return_type := res;
					    push_types_stack ();
					    match params with
						T_PROTOTYPE (l, _) ->
						List.iter (fun (loc, x, qtyp) ->
							   ignore (declare_var CQ_DEFAULT loc
									       x qtyp)) l)
	      | _ -> error (Some loc) ("parameters not given for declaration of function " ^ var));
	  loc, var, typ) }

cq_function_declarator : 
          function_declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "cq_function_declarator : function_declarator";
              $1, false }

	| STATIC function_declarator 
            { 
              (* DEBUG *) Debug.pRINT_DEBUG_PARSE "cq_function_declarator : STATIC function_declarator";

              (** On ajoute des éléments dans le contexte. **)

              let (_, var, _) = $2 in

	      ( context := var :: (!context);
	        $2, true) }
	;

function_definition
        : cq_function_declarator compound_statement
	{ 
          (* DEBUG *) Debug.pRINT_DEBUG_PARSE "function_definition : cq_function_declarator compound_statement";
          let (loc, var, typ), static = $1 in
	  (pop_types_stack ();

	   if static then 
             begin
               (* On verifie que !context n'est pas vide. *)
               if ((!context)= []) then ()
               else
                 context := List.tl (!context);
             end
               else ();

	   declare_fun static loc var typ $2) }
	;

/*
        : declaration_specifiers declarator declaration_list compound_statement
        | declaration_specifiers declarator compound_statement
        | declarator declaration_list compound_statement
        | declarator compound_statement
        ;
*/

%%
