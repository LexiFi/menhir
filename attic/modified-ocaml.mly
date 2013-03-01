// TEMPORARY %inline sur tous les nt a une seule production
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id: menhir.mly,v 1.1 2005/12/18 17:00:47 fpottier Exp $ */

/* The parser definition */

%{
open Parsing
open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_rloc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_rloc() }
let mkmty d =
  { pmty_desc = d; pmty_loc = symbol_rloc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_rloc() }
let mkmod d =
  { pmod_desc = d; pmod_loc = symbol_rloc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_rloc() }
let mkfield d =
  { pfield_desc = d; pfield_loc = symbol_rloc() }
let mkclass d =
  { pcl_desc = d; pcl_loc = symbol_rloc() }
let mkcty d =
  { pcty_desc = d; pcty_loc = symbol_rloc() }

let reloc_pat x = { x with ppat_loc = symbol_rloc () };;
let reloc_exp x = { x with pexp_loc = symbol_rloc () };;

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitely in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -stypes option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp d = { pexp_desc = d; pexp_loc = symbol_gloc () };;
let ghpat d = { ppat_desc = d; ppat_loc = symbol_gloc () };;
let ghtyp d = { ptyp_desc = d; ptyp_loc = symbol_gloc () };;

let mkassert e =
  match e with
  | {pexp_desc = Pexp_construct (Lident "false", None, false) } ->
         mkexp (Pexp_assertfalse)
  | _ -> mkexp (Pexp_assert (e))
;;

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp(Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp(Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp(Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | _, Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let rec mktailexp = function
    [] ->
      ghexp(Pexp_construct(Lident "[]", None, false))
  | e1 :: el ->
      let exp_el = mktailexp el in
      let l = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {pexp_desc = Pexp_tuple [e1; exp_el]; pexp_loc = l} in
      {pexp_desc = Pexp_construct(Lident "::", Some arg, false); pexp_loc = l}

let rec mktailpat = function
    [] ->
      ghpat(Ppat_construct(Lident "[]", None, false))
  | p1 :: pl ->
      let pat_pl = mktailpat pl in
      let l = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {ppat_desc = Ppat_tuple [p1; pat_pl]; ppat_loc = l} in
      {ppat_desc = Ppat_construct(Lident "::", Some arg, false); ppat_loc = l}

let ghstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = {e.pexp_loc with loc_ghost = true} }

let array_function str name =
  Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name))

let rec deep_mkrangepat c1 c2 =
  if c1 = c2 then ghpat(Ppat_constant(Const_char c1)) else
  ghpat(Ppat_or(ghpat(Ppat_constant(Const_char c1)),
                deep_mkrangepat (Char.chr(Char.code c1 + 1)) c2))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(Const_char c1)) else
  reloc_pat (deep_mkrangepat c1 c2)

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

let bigarray_function str name =
  Ldot(Ldot(Lident "Bigarray", str), name)

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist} -> explist
  | exp -> [exp]

let bigarray_get arr arg =
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" "get")),
                       ["", arr; "", c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" "get")),
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" "get")),
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       ["", arr; "", ghexp(Pexp_array coords)]))

let bigarray_set arr arg newval =
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" "set")),
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" "set")),
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" "set")),
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       ["", arr; 
                        "", ghexp(Pexp_array coords);
                        "", newval]))
%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
//%token GREATERRBRACKET // TEMPORARY
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PLUS
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
//%token QUESTIONQUESTION // TEMPORARY
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/open_tuple(expr) (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file

%%

/* Entry points */

implementation:
    structure EOF                        { $1 }
;
interface:
    items = signature EOF                { items }
;
toplevel_phrase:
    items = structure_item+ SEMISEMI     { Ptop_def items }
  | seq_expr SEMISEMI                    { Ptop_def[ghstrexp $1] }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
use_file:
    use_file_tail                        { $1 }
  | seq_expr use_file_tail               { Ptop_def[ghstrexp $1] :: $2 }
;
use_file_tail:
    EOF                                         { [] }
  | SEMISEMI EOF                                { [] }
  | SEMISEMI seq_expr use_file_tail             { Ptop_def[ghstrexp $2] :: $3 }
  | SEMISEMI structure_item use_file_tail       { Ptop_def[$2] :: $3 }
  | SEMISEMI toplevel_directive use_file_tail   { $2 :: $3 }
  | structure_item use_file_tail                { Ptop_def[$1] :: $2 }
  | toplevel_directive use_file_tail            { $1 :: $2 }
;

/* Module expressions */

%inline raw_module_expr:
    mod_longident
      { Pmod_ident $1 }
  | STRUCT structure END
      { Pmod_structure($2) }
  | STRUCT structure error
      { unclosed "struct" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_expr
      { Pmod_functor($3, $5, $8) }
  | module_expr LPAREN module_expr RPAREN
      { Pmod_apply($1, $3) }
  | module_expr LPAREN module_expr error
      { unclosed "(" 2 ")" 4 }
  | LPAREN module_expr COLON module_type RPAREN
      { Pmod_constraint($2, $4) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" 1 ")" 5 }

module_expr:
    m = raw_module_expr
      { mkmod m }
  | LPAREN m = module_expr RPAREN
      { m }
  | LPAREN module_expr error
      { unclosed "(" 1 ")" 3 }

structure:
    structure_tail                              { $1 }
  | seq_expr structure_tail                     { ghstrexp $1 :: $2 }
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI                                    { [] }
  | SEMISEMI seq_expr structure_tail            { ghstrexp $2 :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
    LET flag = rec_flag bs = let_bindings
      { match bs with
          [{ppat_desc = Ppat_any}, exp] -> mkstr(Pstr_eval exp)
        | _ -> mkstr(Pstr_value(flag, bs)) }
  | EXTERNAL x = val_ident_colon t = core_type EQUAL ps = STRING+
      { mkstr(Pstr_primitive(x, {pval_type = t; pval_prim = ps})) }
  | TYPE ds = separated_nonempty_list(AND, type_declaration)
      { mkstr(Pstr_type ds) }
  | EXCEPTION id = UIDENT args = constructor_arguments
      { mkstr(Pstr_exception(id, args)) }
  | EXCEPTION UIDENT EQUAL constr_longident
      { mkstr(Pstr_exn_rebind($2, $4)) }
  | MODULE UIDENT module_binding
      { mkstr(Pstr_module($2, $3)) }
  | MODULE REC bs = separated_nonempty_list(AND, module_rec_binding)
      { mkstr(Pstr_recmodule bs) }
  | MODULE TYPE ident EQUAL module_type
      { mkstr(Pstr_modtype($3, $5)) }
  | OPEN mod_longident
      { mkstr(Pstr_open $2) }
  | CLASS ds = separated_nonempty_list(AND, class_declaration)
      { mkstr(Pstr_class ds) }
  | CLASS TYPE ds = separated_nonempty_list(AND, class_type_declaration)
      { mkstr(Pstr_class_type ds) }
  | INCLUDE module_expr
      { mkstr(Pstr_include $2) }
;
module_binding:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { mkmod(Pmod_constraint($4, $2)) }
  | LPAREN UIDENT COLON module_type RPAREN module_binding
      { mkmod(Pmod_functor($2, $4, $6)) }
;
module_rec_binding:
    UIDENT COLON module_type EQUAL module_expr    { ($1, $3, $5) }
;

/* Module types */

module_type:
    mty_longident
      { mkmty(Pmty_ident $1) }
  | SIG items = signature END
      { mkmty(Pmty_signature items) }
  | SIG signature error
      { unclosed "sig" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_type
      %prec below_WITH
      { mkmty(Pmty_functor($3, $5, $8)) }
  | mty = module_type WITH cs = separated_nonempty_list(AND, with_constraint)
      { mkmty(Pmty_with(mty, cs)) }
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
;
%inline signature:
    items = terminated(signature_item, SEMISEMI?)*
      { items }

signature_item:
    VAL val_ident_colon core_type
      { mksig(Psig_value($2, {pval_type = $3; pval_prim = []})) }
  | EXTERNAL x = val_ident_colon t = core_type EQUAL ps = STRING+
      { mksig(Psig_value(x, {pval_type = t; pval_prim = ps})) }
  | TYPE ds = separated_nonempty_list(AND, type_declaration)
      { mksig(Psig_type ds) }
  | EXCEPTION id = UIDENT args = constructor_arguments
      { mksig(Psig_exception(id, args)) }
  | MODULE UIDENT module_declaration
      { mksig(Psig_module($2, $3)) }
  | MODULE REC ds = separated_nonempty_list(AND, module_rec_declaration)
      { mksig(Psig_recmodule ds) }
  | MODULE TYPE ident
      { mksig(Psig_modtype($3, Pmodtype_abstract)) }
  | MODULE TYPE ident EQUAL module_type
      { mksig(Psig_modtype($3, Pmodtype_manifest $5)) }
  | OPEN mod_longident
      { mksig(Psig_open $2) }
  | INCLUDE module_type
      { mksig(Psig_include $2) }
  | CLASS ds = separated_nonempty_list(AND, class_description)
      { mksig(Psig_class ds) }
  | CLASS TYPE ds = separated_nonempty_list(AND, class_type_declaration)
      { mksig(Psig_class_type ds) }
;

module_declaration:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor($2, $4, $6)) }
;
module_rec_declaration:
    UIDENT COLON module_type                            { ($1, $3) }
;

/* Class expressions */

class_declaration:
    virtual_flag class_type_parameters LIDENT class_fun_binding
      { let params, variance = List.split (fst $2) in
        {pci_virt = $1; pci_params = params, snd $2;
         pci_name = $3; pci_expr = $4; pci_variance = variance;
         pci_loc = symbol_rloc ()} }
;
class_fun_binding:
    EQUAL class_expr
      { $2 }
  | COLON class_type EQUAL class_expr
      { mkclass(Pcl_constraint($4, $2)) }
  | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
;
class_type_parameters:
    /*empty*/
      { [], symbol_gloc () }
  | LBRACKET ps = separated_nonempty_list(COMMA, type_parameter) RBRACKET
      { ps, symbol_rloc () }
;
class_fun_def:
    labeled_simple_pattern MINUSGREATER class_expr
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $3)) }
  | labeled_simple_pattern class_fun_def
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
;
class_expr:
    class_simple_expr
      { $1 }
  | FUN class_fun_def
      { $2 }
  | e = class_simple_expr es = labeled_simple_expr+
      { mkclass(Pcl_apply(e, es)) }
  | LET flag = rec_flag bs = let_bindings IN e = class_expr
      { mkclass(Pcl_let (flag, bs, e)) }
;
class_simple_expr:
    LBRACKET ts = core_type_comma_list RBRACKET id = class_longident
      { mkclass(Pcl_constr(id, ts)) }
  | class_longident
      { mkclass(Pcl_constr($1, [])) }
  | OBJECT class_structure END
      { mkclass(Pcl_structure($2)) }
  | OBJECT class_structure error
      { unclosed "object" 1 "end" 3 }
  | LPAREN class_expr COLON class_type RPAREN
      { mkclass(Pcl_constraint($2, $4)) }
  | LPAREN class_expr COLON class_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN class_expr RPAREN
      { $2 }
  | LPAREN class_expr error
      { unclosed "(" 1 ")" 3 }
;
class_structure:
    class_self_pattern class_fields
      { $1, List.rev $2 }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | /* empty */
      { ghpat(Ppat_any) }
;
class_fields:
    /* empty */
      { [] }
  | class_fields INHERIT class_expr preceded(AS, LIDENT)?
      { Pcf_inher ($3, $4) :: $1 }
  | class_fields VAL value
      { Pcf_val $3 :: $1 }
  | class_fields virtual_method
      { Pcf_virt $2 :: $1 }
  | class_fields concrete_method
      { Pcf_meth $2 :: $1 }
  | class_fields CONSTRAINT constrain
      { Pcf_cstr $3 :: $1 }
  | class_fields INITIALIZER seq_expr
      { Pcf_init $3 :: $1 }
;
value:
        mutable_flag label EQUAL seq_expr
          { $2, $1, $4, symbol_rloc () }
      | mutable_flag label type_constraint EQUAL seq_expr
          { $2, $1, (let (t, t') = $3 in ghexp(Pexp_constraint($5, t, t'))),
            symbol_rloc () }
;
virtual_method:
    METHOD PRIVATE VIRTUAL label COLON poly_type
      { $4, Private, $6, symbol_rloc () }
  | METHOD VIRTUAL private_flag label COLON poly_type
      { $4, $3, $6, symbol_rloc () }
;
concrete_method :
    METHOD private_flag label strict_binding
      { $3, $2, ghexp(Pexp_poly ($4, None)), symbol_rloc () }
  | METHOD private_flag label COLON poly_type EQUAL seq_expr
      { $3, $2, ghexp(Pexp_poly($7,Some $5)), symbol_rloc () }
  | METHOD private_flag LABEL poly_type EQUAL seq_expr
      { $3, $2, ghexp(Pexp_poly($6,Some $4)), symbol_rloc () }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun("?" ^ $2 ,
                       {ptyp_desc = Ptyp_constr(Lident "option", [$4]);
                        ptyp_loc = $4.ptyp_loc},
                       $6)) }
  | OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun("?" ^ $1 ,
                       {ptyp_desc = Ptyp_constr(Lident "option", [$2]);
                        ptyp_loc = $2.ptyp_loc},
                       $4)) }
  | LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun($1, $3, $5)) }
  | simple_core_type_or_tuple MINUSGREATER class_type
      { mkcty(Pcty_fun("", $1, $3)) }
;
class_signature:
    LBRACKET ts = core_type_comma_list RBRACKET id = clty_longident
      { mkcty(Pcty_constr (id, ts)) }
  | clty_longident
      { mkcty(Pcty_constr ($1, [])) }
  | OBJECT class_sig_body END
      { mkcty(Pcty_signature $2) }
  | OBJECT class_sig_body error
      { unclosed "object" 1 "end" 3 }
;
class_sig_body:
    class_self_type class_sig_fields
      { $1, List.rev $2 }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  | /* empty */
      { mktyp(Ptyp_any) }
;
class_sig_fields:
    /* empty */                                 { [] }
  | class_sig_fields INHERIT class_signature    { Pctf_inher $3 :: $1 }
  | class_sig_fields VAL value_type             { Pctf_val   $3 :: $1 }
  | class_sig_fields virtual_method             { Pctf_virt  $2 :: $1 }
  | class_sig_fields method_type                { Pctf_meth  $2 :: $1 }
  | class_sig_fields CONSTRAINT constrain       { Pctf_cstr  $3 :: $1 }
;
value_type:
    mutable_flag label COLON core_type
      { $2, $1, Some $4, symbol_rloc () }
;
method_type:
    METHOD private_flag label COLON poly_type
      { $3, $2, $5, symbol_rloc () }
;
constrain:
        core_type EQUAL core_type          { $1, $3, symbol_rloc () }
;
class_description:
    virtual_flag class_type_parameters LIDENT COLON class_type
      { let params, variance = List.split (fst $2) in
        {pci_virt = $1; pci_params = params, snd $2;
         pci_name = $3; pci_expr = $5; pci_variance = variance;
         pci_loc = symbol_rloc ()} }
;
class_type_declaration:
    virtual_flag class_type_parameters LIDENT EQUAL class_signature
      { let params, variance = List.split (fst $2) in
        {pci_virt = $1; pci_params = params, snd $2;
         pci_name = $3; pci_expr = $5; pci_variance = variance;
         pci_loc = symbol_rloc ()} }
;

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { reloc_exp $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern preceded(EQUAL, seq_expr)? RPAREN
      { ("?" ^ fst $3, $4, snd $3) }
  | QUESTION label_var
      { ("?" ^ fst $2, None, snd $2) }
  | OPTLABEL LPAREN let_pattern preceded(EQUAL, seq_expr)? RPAREN
      { ("?" ^ $1, $4, $3) }
  | OPTLABEL pattern_var
      { ("?" ^ $1, None, $2) }
  | TILDE LPAREN label_let_pattern RPAREN
      { (fst $3, None, snd $3) }
  | TILDE label_var
      { (fst $2, None, snd $2) }
  | LABEL simple_pattern
      { ($1, None, $2) }
  | simple_pattern
      { ("", None, $1) }
;
pattern_var:
    LIDENT    { mkpat(Ppat_var $1) }
;
label_let_pattern:
    label_var
      { $1 }
  | label_var COLON core_type
      { let (lab, pat) = $1 in (lab, mkpat(Ppat_constraint(pat, $3))) }
;
label_var:
    LIDENT    { ($1, mkpat(Ppat_var $1)) }
;
let_pattern:
    pattern
      { $1 }
  | pattern COLON core_type
      { mkpat(Ppat_constraint($1, $3)) }
;
expr:
    simple_expr %prec below_SHARP
      { $1 }
  | e = simple_expr es = labeled_simple_expr+
      { mkexp(Pexp_apply(e, es)) }
  | LET flag = rec_flag bs = let_bindings IN e = seq_expr
      { mkexp(Pexp_let(flag, bs, e)) }
  | LET MODULE UIDENT module_binding IN seq_expr
      { mkexp(Pexp_letmodule($3, $4, $6)) }
  | FUNCTION BAR? cases = match_cases
      { mkexp(Pexp_function("", None, cases)) }
  | FUN labeled_simple_pattern fun_def
      { let (l,o,p) = $2 in mkexp(Pexp_function(l, o, [p, $3])) }
  | MATCH e = seq_expr WITH BAR? cases = match_cases
      { mkexp(Pexp_match(e, cases)) }
  | TRY e = seq_expr WITH BAR? cases = match_cases
      { mkexp(Pexp_try(e, cases)) }
  | TRY seq_expr WITH error
      { syntax_error() }
  | es = open_tuple(expr) %prec below_COMMA
      { mkexp(Pexp_tuple (List.rev es)) }
  | constr_longident simple_expr %prec below_SHARP
      { mkexp(Pexp_construct($1, Some $2, false)) }
  | name_tag simple_expr %prec below_SHARP
      { mkexp(Pexp_variant($1, Some $2)) }
  | IF seq_expr THEN expr ELSE expr
      { mkexp(Pexp_ifthenelse($2, $4, Some $6)) }
  | IF seq_expr THEN expr
      { mkexp(Pexp_ifthenelse($2, $4, None)) }
  | WHILE seq_expr DO seq_expr DONE
      { mkexp(Pexp_while($2, $4)) }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { mkexp(Pexp_for($2, $4, $6, $5, $8)) }
  | expr COLONCOLON expr
      { mkexp(Pexp_construct(Lident "::",
                             Some(ghexp(Pexp_tuple[$1;$3])),
                             false)) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { mkexp(Pexp_construct(Lident "::",
                             Some(ghexp(Pexp_tuple[$5;$7])),
                             false)) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 "+" $3 }
  | expr MINUS expr
      { mkinfix $1 "-" $3 }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                         ["",$1; "",$4; "",$7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "set")),
                         ["",$1; "",$4; "",$7])) }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { bigarray_set $1 $4 $7 }
  | label LESSMINUS expr
      { mkexp(Pexp_setinstvar($1, $3)) }
  | ASSERT simple_expr %prec below_SHARP
      { mkassert $2 }
  | LAZY simple_expr %prec below_SHARP
      { mkexp (Pexp_lazy ($2)) }
  | OBJECT class_structure END
      { mkexp (Pexp_object($2)) }
  | OBJECT class_structure error
      { unclosed "object" 1 "end" 3 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident %prec prec_constant_constructor
      { mkexp(Pexp_construct($1, None, false)) }
  | name_tag %prec prec_constant_constructor
      { mkexp(Pexp_variant($1, None)) }
  | LPAREN seq_expr RPAREN
      { reloc_exp $2 }
  | LPAREN seq_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN seq_expr END
      { reloc_exp $2 }
  | BEGIN END
      { mkexp (Pexp_construct (Lident "()", None, false)) }
  | BEGIN seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN seq_expr type_constraint RPAREN
      { let (t, t') = $3 in mkexp(Pexp_constraint($2, t, t')) }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, $3)) }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                         ["",$1; "",$4])) }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "get")),
                         ["",$1; "",$4])) }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" 3 "]" 5 }
  | simple_expr DOT LBRACE expr RBRACE
      { bigarray_get $1 $4 }
  | simple_expr DOT LBRACE open_tuple(expr) error
      { unclosed "{" 3 "}" 5 }
  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in mkexp(Pexp_record(fields, exten)) }
  | LBRACE record_expr error
      { unclosed "{" 1 "}" 3 }
  | LBRACKETBAR es = semi_list(expr) BARRBRACKET
      { mkexp(Pexp_array es) }
  | LBRACKETBAR semi_list(expr) error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp(Pexp_array []) }
  | LBRACKET es = semi_list(expr) RBRACKET
      { reloc_exp (mktailexp es) }
  | LBRACKET semi_list(expr) error
      { unclosed "[" 1 "]" 4 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, ["",$2])) }
  | NEW class_longident
      { mkexp(Pexp_new($2)) }
  | LBRACELESS fields = field_expr_list GREATERRBRACE
      { mkexp(Pexp_override fields) }
  | LBRACELESS field_expr_list error
      { unclosed "{<" 1 ">}" 4 }
  | LBRACELESS GREATERRBRACE
      { mkexp(Pexp_override []) }
  | simple_expr SHARP label
      { mkexp(Pexp_send($1, $3)) }
;
%inline semi_list(X):
    xs = separated_nonempty_list(SEMI, X) SEMI?
      { xs }

labeled_simple_expr:
    simple_expr %prec below_SHARP
      { ("", $1) }
  | LABEL simple_expr %prec below_SHARP
      { ($1, $2) }
  | TILDE label_ident
      { $2 }
  | QUESTION label_ident
      { ("?" ^ fst $2, snd $2) }
  | OPTLABEL simple_expr %prec below_SHARP
      { ("?" ^ $1, $2) }
;
label_ident:
    LIDENT   { ($1, mkexp(Pexp_ident(Lident $1))) }
;
%inline let_bindings:
    bs = separated_nonempty_list(AND, let_binding)
      { bs}

let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_var $1; ppat_loc = rhs_loc 1}, $2) }
  | pattern EQUAL seq_expr
      { ($1, $3) }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { let (t, t') = $1 in ghexp(Pexp_constraint($3, t, t')) }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern fun_binding
      { let (l, o, p) = $1 in ghexp(Pexp_function(l, o, [p, $2])) }
;
%inline match_cases:
    cases = separated_nonempty_list(BAR, pair(pattern, match_action))
      { cases }

fun_def:
    match_action                                { $1 }
  | labeled_simple_pattern fun_def
      { let (l,o,p) = $1 in ghexp(Pexp_function(l, o, [p, $2])) }
;
match_action:
    MINUSGREATER seq_expr                       { $2 }
  | WHEN seq_expr MINUSGREATER seq_expr         { mkexp(Pexp_when($2, $4)) }
;
open_tuple(X): // A list of at least two X's, separated with commas.
    xs = open_tuple(X) COMMA x = X
      { x :: xs }
  | x1 = X COMMA x2 = X
      { [ x2; x1 ] }

record_expr:
    base = ioption(terminated(simple_expr, WITH)) fields = lbl_list(expr)
      { (base, fields) }

%inline field_expr_list:
    fields = separated_nonempty_list(SEMI, separated_pair(label, EQUAL, expr)) SEMI?
      { fields }

type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, $3)) }
  | ps = open_tuple(pattern)  %prec below_COMMA
      { mkpat(Ppat_tuple (List.rev ps)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2, false)) }
  | name_tag pattern %prec prec_constr_appl
      { mkpat(Ppat_variant($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[$1;$3])),
                             false)) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { mkpat(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[$5;$7])),
                             false)) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct($1, None, false)) }
  | name_tag
      { mkpat(Ppat_variant($1, None)) }
  | SHARP type_longident
      { mkpat(Ppat_type $2) }
  | LBRACE ps = lbl_list(pattern) RBRACE
      { mkpat(Ppat_record ps) }
  | LBRACE lbl_list(pattern) error
      { unclosed "{" 1 "}" 4 }
  | LBRACKET ps = semi_list(pattern) RBRACKET
      { reloc_pat (mktailpat ps) }
  | LBRACKET semi_list(pattern) error
      { unclosed "[" 1 "]" 4 }
  | LBRACKETBAR ps = semi_list(pattern) BARRBRACKET
      { mkpat(Ppat_array ps) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LBRACKETBAR semi_list(pattern) error
      { unclosed "[|" 1 "|]" 4 }
  | LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
;

%inline lbl_list(X):
  fields = separated_nonempty_list(SEMI, separated_pair(label_longident, EQUAL, X)) SEMI?
    { fields }

/* Type declarations */

type_declaration:
    psv = type_parameters id = LIDENT km = type_kind cs = constraints
      { let (params, variance) = List.split psv in
        let (kind, manifest) = km in
        (id, {ptype_params = params;
              ptype_cstrs = cs;
              ptype_kind = kind;
              ptype_manifest = manifest;
              ptype_variance = variance;
              ptype_loc = symbol_rloc()}) }
;
%inline constraints:
    cs = preceded(CONSTRAINT, constrain)*
      { cs }

type_kind:
    /*empty*/
      { (Ptype_abstract, None) }
  | EQUAL core_type
      { (Ptype_abstract, Some $2) }
  | EQUAL ds = constructor_declarations
      { (Ptype_variant(ds, Public), None) }
  | EQUAL PRIVATE ds = constructor_declarations
      { (Ptype_variant(ds, Private), None) }
  | EQUAL flag = private_flag BAR ds = constructor_declarations
      { (Ptype_variant(ds, flag), None) }
  | EQUAL flag = private_flag LBRACE ds = semi_list(label_declaration) RBRACE
      { (Ptype_record(ds, flag), None) }
  | EQUAL t = core_type EQUAL flag = private_flag BAR? ds = constructor_declarations
      { (Ptype_variant(ds, flag), Some t) }
  | EQUAL t = core_type EQUAL flag = private_flag LBRACE ds = semi_list(label_declaration) RBRACE
      { (Ptype_record(ds, flag), Some t) }
  | EQUAL PRIVATE core_type
      { (Ptype_private, Some $3) }
;
type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN ps = separated_nonempty_list(COMMA, type_parameter) RPAREN
      { ps }
;
type_parameter:
    type_variance QUOTE ident                   { $3, $1 }
;
type_variance:
    /* empty */                                 { false, false }
  | PLUS                                        { true, false }
  | MINUS                                       { false, true }
;
%inline constructor_declarations:
    ds = separated_nonempty_list(BAR, constructor_declaration)
      { ds }

constructor_declaration:
    id = constr_ident args = constructor_arguments
      { (id, args, symbol_rloc()) }
;
%inline constructor_arguments:
    args = loption(preceded(OF, core_type_list))
      { args }

label_declaration:
    mutable_flag label COLON poly_type          { ($2, $1, $4, symbol_rloc()) }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
    TYPE
    psv = type_parameters
    id = label_longident
    kind = with_type_binder
    t = core_type
    cs = constraints
      { let params, variance = List.split psv in
        (id, Pwith_type {ptype_params = params;
                         ptype_cstrs = cs;
                         ptype_kind = kind;
                         ptype_manifest = Some t;
                         ptype_variance = variance;
                         ptype_loc = symbol_rloc()}) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | MODULE mod_longident EQUAL mod_ext_longident
      { ($2, Pwith_module $4) }
;
with_type_binder:
    EQUAL          { Ptype_abstract }
  | EQUAL PRIVATE  { Ptype_private }
;

/* Polymorphic types */

poly_type:
        core_type
          { mktyp(Ptyp_poly([], $1)) }
      | tyvars = preceded(QUOTE, ident)+ DOT ty = core_type
          { mktyp(Ptyp_poly(tyvars, ty)) }
;

/* Core types */

core_type:
    core_type2
      { $1 }
  | core_type2 AS QUOTE ident
      { mktyp(Ptyp_alias($1, $4)) }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $2 ,
               {ptyp_desc = Ptyp_constr(Lident "option", [$4]);
                ptyp_loc = $4.ptyp_loc}, $6)) }
  | OPTLABEL core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $1 ,
               {ptyp_desc = Ptyp_constr(Lident "option", [$2]);
                ptyp_loc = $2.ptyp_loc}, $4)) }
  | LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow($1, $3, $5)) }
  | core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("", $1, $3)) }
;

simple_core_type:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN ts = core_type_comma_list RPAREN %prec below_SHARP
      { match ts with [sty] -> sty | _ -> raise Parse_error }
;
simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type2 type_longident
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN ts = core_type_comma_list RPAREN id = type_longident
      { mktyp(Ptyp_constr(id, ts)) }
  | LESS meth_list GREATER
      { mktyp(Ptyp_object $2) }
  | LESS GREATER
      { mktyp(Ptyp_object []) }
  | SHARP class_longident opt_present
      { mktyp(Ptyp_class($2, [], $3)) }
  | simple_core_type2 SHARP class_longident opt_present
      { mktyp(Ptyp_class($3, [$1], $4)) }
  | LPAREN ts = core_type_comma_list RPAREN SHARP id = class_longident tags = opt_present
      { mktyp(Ptyp_class(id, ts, tags)) }
  | LBRACKET tag_field RBRACKET
      { mktyp(Ptyp_variant([$2], true, None)) }
  | LBRACKET BAR fs = row_field_list RBRACKET
      { mktyp(Ptyp_variant(fs, true, None)) }
  | LBRACKET f = row_field BAR fs = row_field_list RBRACKET
      { mktyp(Ptyp_variant(f :: fs, true, None)) }
  | LBRACKETGREATER BAR? fs = row_field_list RBRACKET
      { mktyp(Ptyp_variant(fs, false, None)) }
  | LBRACKETGREATER RBRACKET
      { mktyp(Ptyp_variant([], false, None)) }
  | LBRACKETLESS BAR? fs = row_field_list RBRACKET
      { mktyp(Ptyp_variant(fs, true, Some [])) }
  | LBRACKETLESS BAR? fs = row_field_list GREATER tags = name_tag+ RBRACKET
      { mktyp(Ptyp_variant(fs, true, Some tags)) }
;
%inline row_field_list:
    fs = separated_nonempty_list(BAR, row_field)
      { fs }

row_field:
    tag_field                                   { $1 }
  | simple_core_type2                           { Rinherit $1 }
;
tag_field:
    tag = name_tag OF a = boption(AMPERSAND) ts = separated_nonempty_list(AMPERSAND, core_type)
      { Rtag (tag, a, ts) }
  | name_tag
      { Rtag ($1, true, []) }
;
opt_present:
    LBRACKETGREATER tags = name_tag+ RBRACKET      { tags }
  | /* empty */                                    { [] }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | t = simple_core_type STAR ts = core_type_list
      { mktyp(Ptyp_tuple(t :: ts)) }
;
%inline core_type_comma_list:
    ts = separated_nonempty_list(COMMA, core_type)
      { ts }

%inline core_type_list:
    ts = separated_nonempty_list(STAR, simple_core_type)
      { ts }

meth_list:
    field SEMI meth_list                        { $1 :: $3 }
  | field SEMI?                              { [$1] }
  | DOTDOT                                      { [mkfield Pfield_var] }
;
field:
    label COLON poly_type                       { mkfield(Pfield($1, $3)) }
;
label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
  | INT32                                       { Const_int32 $1 }
  | INT64                                       { Const_int64 $1 }
  | NATIVEINT                                   { Const_nativeint $1 }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { Const_int(- $2) }
  | MINUS FLOAT                                 { Const_float("-" ^ $2) }
  | MINUS INT32                                 { Const_int32(Int32.neg $2) }
  | MINUS INT64                                 { Const_int64(Int64.neg $2) }
  | MINUS NATIVEINT                             { Const_nativeint(Nativeint.neg $2) }
;
/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
val_ident_colon:
    LIDENT COLON                                { $1 }
  | LPAREN operator RPAREN COLON                { $2 }
  | LABEL                                       { $1 }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { Lapply($1, $3) }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
clty_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string $3) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident FALSE           { Ptop_dir($2, Pdir_bool false) }
  | SHARP ident TRUE            { Ptop_dir($2, Pdir_bool true) }
;

/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
%%
