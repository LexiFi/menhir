%start implementation
%start interface
%start toplevel_phrase
%start use_file
%token WHILE
%token WHEN
%token VIRTUAL
%token VAL
%token UNDERSCORE
%token TYPE
%token TRY
%token TO
%token TILDE
%token STRUCT
%token SIG
%token SEMISEMI
%token RPAREN
%token REC
%token RBRACKET
%token RBRACE
%token QUOTE
%token QUESTIONQUESTION
%token QUESTION
%token PRIVATE
%token <string> OPTLABEL
%token OPEN
%token OF
%token OBJECT
%token MUTABLE
%token MODULE
%token METHOD
%token MATCH
%token LBRACKETLESS
%token LBRACKETGREATER
%token LAZY
%token <string> LABEL
%token INITIALIZER
%token INHERIT
%token INCLUDE
%token IF
%token GREATERRBRACKET
%token GREATERRBRACE
%token FUNCTOR
%token FUN
%token FOR
%token EXTERNAL
%token EXCEPTION
%token EOF
%token END
%token DOWNTO
%token DOTDOT
%token DONE
%token DO
%token CONSTRAINT
%token COLONGREATER
%token COLON
%token CLASS
%token BARRBRACKET
%token ASSERT
%token IN
%token SEMI
%token LET
%token WITH
%token FUNCTION
%token AND
%token THEN
%token ELSE
%token LESSMINUS
%token COLONEQUAL
%token AS
%token BAR
%token COMMA
%token MINUSGREATER
%token OR
%token BARBAR
%token AMPERSAND
%token AMPERAMPER
%token LESS
%token <string> INFIXOP0
%token GREATER
%token EQUAL
%token <string> INFIXOP1
%token COLONCOLON
%token PLUS
%token MINUSDOT
%token MINUS
%token <string> INFIXOP2
%token STAR
%token <string> INFIXOP3
%token <string> INFIXOP4
%token SHARP
%token DOT
%token <string> UIDENT
%token TRUE
%token <string> STRING
%token <string> PREFIXOP
%token NEW
%token <nativeint> NATIVEINT
%token LPAREN
%token <string> LIDENT
%token LBRACKETBAR
%token LBRACKET
%token LBRACELESS
%token LBRACE
%token <int64> INT64
%token <int32> INT32
%token <int> INT
%token <string> FLOAT
%token FALSE
%token <char> CHAR
%token BEGIN
%token BACKQUOTE
%nonassoc IN 
%nonassoc below_SEMI 
%nonassoc SEMI 
%nonassoc LET 
%nonassoc below_WITH 
%nonassoc WITH FUNCTION 
%nonassoc AND 
%nonassoc THEN 
%nonassoc ELSE 
%nonassoc LESSMINUS 
%right COLONEQUAL 
%nonassoc AS 
%left BAR 
%nonassoc below_COMMA 
%left COMMA 
%right MINUSGREATER 
%right OR BARBAR 
%right AMPERSAND AMPERAMPER 
%nonassoc below_EQUAL 
%left LESS INFIXOP0 GREATER EQUAL 
%right INFIXOP1 
%right COLONCOLON 
%left PLUS MINUSDOT MINUS INFIXOP2 
%left STAR INFIXOP3 
%right INFIXOP4 
%nonassoc prec_unary_minus 
%nonassoc prec_constant_constructor 
%nonassoc prec_constr_appl 
%nonassoc below_SHARP 
%nonassoc SHARP 
%nonassoc below_DOT 
%nonassoc DOT 
%nonassoc UIDENT TRUE STRING PREFIXOP NEW NATIVEINT LPAREN LIDENT LBRACKETBAR LBRACKET LBRACELESS LBRACE INT64 INT32 INT FLOAT FALSE CHAR BEGIN BACKQUOTE 
%type <unit> implementation
%type <unit> interface
%type <unit> toplevel_phrase
%type <unit> use_file
%%

implementation:
| structure EOF
    {()}

interface:
| signature EOF
    {()}

toplevel_phrase:
| top_structure SEMISEMI
    {()}
| seq_expr SEMISEMI
    {()}
| toplevel_directive SEMISEMI
    {()}
| EOF
    {()}

top_structure:
| structure_item
    {()}
| structure_item top_structure
    {()}

use_file:
| use_file_tail
    {()}
| seq_expr use_file_tail
    {()}

use_file_tail:
| EOF
    {()}
| SEMISEMI EOF
    {()}
| SEMISEMI seq_expr use_file_tail
    {()}
| SEMISEMI structure_item use_file_tail
    {()}
| SEMISEMI toplevel_directive use_file_tail
    {()}
| structure_item use_file_tail
    {()}
| toplevel_directive use_file_tail
    {()}

module_expr:
| mod_longident
    {()}
| STRUCT structure END
    {()}
| STRUCT structure error
    {()}
| FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_expr
    {()}
| module_expr LPAREN module_expr RPAREN
    {()}
| module_expr LPAREN module_expr error
    {()}
| LPAREN module_expr COLON module_type RPAREN
    {()}
| LPAREN module_expr COLON module_type error
    {()}
| LPAREN module_expr RPAREN
    {()}
| LPAREN module_expr error
    {()}

structure:
| structure_tail
    {()}
| seq_expr structure_tail
    {()}

structure_tail:
| 
    {()}
| SEMISEMI
    {()}
| SEMISEMI seq_expr structure_tail
    {()}
| SEMISEMI structure_item structure_tail
    {()}
| structure_item structure_tail
    {()}

structure_item:
| LET rec_flag let_bindings
    {()}
| EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
    {()}
| TYPE type_declarations
    {()}
| EXCEPTION UIDENT constructor_arguments
    {()}
| EXCEPTION UIDENT EQUAL constr_longident
    {()}
| MODULE UIDENT module_binding
    {()}
| MODULE REC module_rec_bindings
    {()}
| MODULE TYPE ident EQUAL module_type
    {()}
| OPEN mod_longident
    {()}
| CLASS class_declarations
    {()}
| CLASS TYPE class_type_declarations
    {()}
| INCLUDE module_expr
    {()}

module_binding:
| EQUAL module_expr
    {()}
| COLON module_type EQUAL module_expr
    {()}
| LPAREN UIDENT COLON module_type RPAREN module_binding
    {()}

module_rec_bindings:
| module_rec_binding
    {()}
| module_rec_bindings AND module_rec_binding
    {()}

module_rec_binding:
| UIDENT COLON module_type EQUAL module_expr
    {()}

module_type:
| mty_longident
    {()}
| SIG signature END
    {()}
| SIG signature error
    {()}
| FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_type %prec below_WITH
    {()}
| module_type WITH with_constraints
    {()}
| LPAREN module_type RPAREN
    {()}
| LPAREN module_type error
    {()}

signature:
| 
    {()}
| signature signature_item
    {()}
| signature signature_item SEMISEMI
    {()}

signature_item:
| VAL val_ident COLON core_type
    {()}
| EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
    {()}
| TYPE type_declarations
    {()}
| EXCEPTION UIDENT constructor_arguments
    {()}
| MODULE UIDENT module_declaration
    {()}
| MODULE REC module_rec_declarations
    {()}
| MODULE TYPE ident
    {()}
| MODULE TYPE ident EQUAL module_type
    {()}
| OPEN mod_longident
    {()}
| INCLUDE module_type
    {()}
| CLASS class_descriptions
    {()}
| CLASS TYPE class_type_declarations
    {()}

module_declaration:
| COLON module_type
    {()}
| LPAREN UIDENT COLON module_type RPAREN module_declaration
    {()}

module_rec_declarations:
| module_rec_declaration
    {()}
| module_rec_declarations AND module_rec_declaration
    {()}

module_rec_declaration:
| UIDENT COLON module_type
    {()}

class_declarations:
| class_declarations AND class_declaration
    {()}
| class_declaration
    {()}

class_declaration:
| virtual_flag class_type_parameters LIDENT class_fun_binding
    {()}

class_fun_binding:
| EQUAL class_expr
    {()}
| COLON class_type EQUAL class_expr
    {()}
| labeled_simple_pattern class_fun_binding
    {()}

class_type_parameters:
| 
    {()}
| LBRACKET type_parameter_list RBRACKET
    {()}

class_fun_def:
| labeled_simple_pattern MINUSGREATER class_expr
    {()}
| labeled_simple_pattern class_fun_def
    {()}

class_expr:
| class_simple_expr
    {()}
| FUN class_fun_def
    {()}
| class_simple_expr simple_labeled_expr_list
    {()}
| LET rec_flag let_bindings IN class_expr
    {()}

class_simple_expr:
| LBRACKET core_type_comma_list RBRACKET class_longident
    {()}
| class_longident
    {()}
| OBJECT class_structure END
    {()}
| OBJECT class_structure error
    {()}
| LPAREN class_expr COLON class_type RPAREN
    {()}
| LPAREN class_expr COLON class_type error
    {()}
| LPAREN class_expr RPAREN
    {()}
| LPAREN class_expr error
    {()}

class_structure:
| class_self_pattern class_fields
    {()}

class_self_pattern:
| LPAREN pattern RPAREN
    {()}
| LPAREN pattern COLON core_type RPAREN
    {()}
| 
    {()}

class_fields:
| 
    {()}
| class_fields INHERIT class_expr parent_binder
    {()}
| class_fields VAL virtual_value
    {()}
| class_fields VAL value
    {()}
| class_fields virtual_method
    {()}
| class_fields concrete_method
    {()}
| class_fields CONSTRAINT constrain
    {()}
| class_fields INITIALIZER seq_expr
    {()}

parent_binder:
| AS LIDENT
    {()}
| 
    {()}

virtual_value:
| MUTABLE VIRTUAL label COLON core_type
    {()}
| VIRTUAL mutable_flag label COLON core_type
    {()}

value:
| mutable_flag label EQUAL seq_expr
    {()}
| mutable_flag label type_constraint EQUAL seq_expr
    {()}

virtual_method:
| METHOD PRIVATE VIRTUAL label COLON poly_type
    {()}
| METHOD VIRTUAL private_flag label COLON poly_type
    {()}

concrete_method:
| METHOD private_flag label strict_binding
    {()}
| METHOD private_flag label COLON poly_type EQUAL seq_expr
    {()}

class_type:
| class_signature
    {()}
| QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
    {()}
| OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
    {()}
| LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
    {()}
| simple_core_type_or_tuple MINUSGREATER class_type
    {()}

class_signature:
| LBRACKET core_type_comma_list RBRACKET clty_longident
    {()}
| clty_longident
    {()}
| OBJECT class_sig_body END
    {()}
| OBJECT class_sig_body error
    {()}

class_sig_body:
| class_self_type class_sig_fields
    {()}

class_self_type:
| LPAREN core_type RPAREN
    {()}
| 
    {()}

class_sig_fields:
| 
    {()}
| class_sig_fields INHERIT class_signature
    {()}
| class_sig_fields VAL value_type
    {()}
| class_sig_fields virtual_method
    {()}
| class_sig_fields method_type
    {()}
| class_sig_fields CONSTRAINT constrain
    {()}

value_type:
| VIRTUAL mutable_flag label COLON core_type
    {()}
| MUTABLE virtual_flag label COLON core_type
    {()}
| label COLON core_type
    {()}

method_type:
| METHOD private_flag label COLON poly_type
    {()}

constrain:
| core_type EQUAL core_type
    {()}

class_descriptions:
| class_descriptions AND class_description
    {()}
| class_description
    {()}

class_description:
| virtual_flag class_type_parameters LIDENT COLON class_type
    {()}

class_type_declarations:
| class_type_declarations AND class_type_declaration
    {()}
| class_type_declaration
    {()}

class_type_declaration:
| virtual_flag class_type_parameters LIDENT EQUAL class_signature
    {()}

seq_expr:
| expr %prec below_SEMI
    {()}
| expr SEMI
    {()}
| expr SEMI seq_expr
    {()}

labeled_simple_pattern:
| QUESTION LPAREN label_let_pattern opt_default RPAREN
    {()}
| QUESTION label_var
    {()}
| OPTLABEL LPAREN let_pattern opt_default RPAREN
    {()}
| OPTLABEL pattern_var
    {()}
| TILDE LPAREN label_let_pattern RPAREN
    {()}
| TILDE label_var
    {()}
| LABEL simple_pattern
    {()}
| simple_pattern
    {()}

pattern_var:
| LIDENT
    {()}
| UNDERSCORE
    {()}

opt_default:
| 
    {()}
| EQUAL seq_expr
    {()}

label_let_pattern:
| label_var
    {()}
| label_var COLON core_type
    {()}

label_var:
| LIDENT
    {()}

let_pattern:
| pattern
    {()}
| pattern COLON core_type
    {()}

expr:
| simple_expr %prec below_SHARP
    {()}
| simple_expr simple_labeled_expr_list
    {()}
| LET rec_flag let_bindings IN seq_expr
    {()}
| LET MODULE UIDENT module_binding IN seq_expr
    {()}
| FUNCTION opt_bar match_cases
    {()}
| FUN labeled_simple_pattern fun_def
    {()}
| MATCH seq_expr WITH opt_bar match_cases
    {()}
| TRY seq_expr WITH opt_bar match_cases
    {()}
| TRY seq_expr WITH error
    {()}
| expr_comma_list %prec below_COMMA
    {()}
| constr_longident simple_expr %prec below_SHARP
    {()}
| name_tag simple_expr %prec below_SHARP
    {()}
| IF seq_expr THEN expr ELSE expr
    {()}
| IF seq_expr THEN expr
    {()}
| WHILE seq_expr DO seq_expr DONE
    {()}
| FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
    {()}
| expr COLONCOLON expr
    {()}
| LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
    {()}
| expr INFIXOP0 expr
    {()}
| expr INFIXOP1 expr
    {()}
| expr INFIXOP2 expr
    {()}
| expr INFIXOP3 expr
    {()}
| expr INFIXOP4 expr
    {()}
| expr PLUS expr
    {()}
| expr MINUS expr
    {()}
| expr MINUSDOT expr
    {()}
| expr STAR expr
    {()}
| expr EQUAL expr
    {()}
| expr LESS expr
    {()}
| expr GREATER expr
    {()}
| expr OR expr
    {()}
| expr BARBAR expr
    {()}
| expr AMPERSAND expr
    {()}
| expr AMPERAMPER expr
    {()}
| expr COLONEQUAL expr
    {()}
| subtractive expr %prec prec_unary_minus
    {()}
| simple_expr DOT label_longident LESSMINUS expr
    {()}
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
    {()}
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
    {()}
| simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
    {()}
| label LESSMINUS expr
    {()}
| ASSERT simple_expr %prec below_SHARP
    {()}
| LAZY simple_expr %prec below_SHARP
    {()}
| OBJECT class_structure END
    {()}
| OBJECT class_structure error
    {()}

simple_expr:
| val_longident
    {()}
| constant
    {()}
| constr_longident %prec prec_constant_constructor
    {()}
| name_tag %prec prec_constant_constructor
    {()}
| LPAREN seq_expr RPAREN
    {()}
| LPAREN seq_expr error
    {()}
| BEGIN seq_expr END
    {()}
| BEGIN END
    {()}
| BEGIN seq_expr error
    {()}
| LPAREN seq_expr type_constraint RPAREN
    {()}
| simple_expr DOT label_longident
    {()}
| simple_expr DOT LPAREN seq_expr RPAREN
    {()}
| simple_expr DOT LPAREN seq_expr error
    {()}
| simple_expr DOT LBRACKET seq_expr RBRACKET
    {()}
| simple_expr DOT LBRACKET seq_expr error
    {()}
| simple_expr DOT LBRACE expr RBRACE
    {()}
| simple_expr DOT LBRACE expr_comma_list error
    {()}
| LBRACE record_expr RBRACE
    {()}
| LBRACE record_expr error
    {()}
| LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
    {()}
| LBRACKETBAR expr_semi_list opt_semi error
    {()}
| LBRACKETBAR BARRBRACKET
    {()}
| LBRACKET expr_semi_list opt_semi RBRACKET
    {()}
| LBRACKET expr_semi_list opt_semi error
    {()}
| PREFIXOP simple_expr
    {()}
| NEW class_longident
    {()}
| LBRACELESS field_expr_list opt_semi GREATERRBRACE
    {()}
| LBRACELESS field_expr_list opt_semi error
    {()}
| LBRACELESS GREATERRBRACE
    {()}
| simple_expr SHARP label
    {()}

simple_labeled_expr_list:
| labeled_simple_expr
    {()}
| simple_labeled_expr_list labeled_simple_expr
    {()}

labeled_simple_expr:
| simple_expr %prec below_SHARP
    {()}
| label_expr
    {()}

label_expr:
| LABEL simple_expr %prec below_SHARP
    {()}
| TILDE label_ident
    {()}
| QUESTION label_ident
    {()}
| OPTLABEL simple_expr %prec below_SHARP
    {()}

label_ident:
| LIDENT
    {()}

let_bindings:
| let_binding
    {()}
| let_bindings AND let_binding
    {()}

let_binding:
| val_ident fun_binding
    {()}
| pattern EQUAL seq_expr
    {()}

fun_binding:
| strict_binding
    {()}
| type_constraint EQUAL seq_expr
    {()}

strict_binding:
| EQUAL seq_expr
    {()}
| labeled_simple_pattern fun_binding
    {()}

match_cases:
| pattern match_action
    {()}
| match_cases BAR pattern match_action
    {()}

fun_def:
| match_action
    {()}
| labeled_simple_pattern fun_def
    {()}

match_action:
| MINUSGREATER seq_expr
    {()}
| WHEN seq_expr MINUSGREATER seq_expr
    {()}

expr_comma_list:
| expr_comma_list COMMA expr
    {()}
| expr COMMA expr
    {()}

record_expr:
| simple_expr WITH lbl_expr_list opt_semi
    {()}
| lbl_expr_list opt_semi
    {()}

lbl_expr_list:
| label_longident EQUAL expr
    {()}
| lbl_expr_list SEMI label_longident EQUAL expr
    {()}

field_expr_list:
| label EQUAL expr
    {()}
| field_expr_list SEMI label EQUAL expr
    {()}

expr_semi_list:
| expr
    {()}
| expr_semi_list SEMI expr
    {()}

type_constraint:
| COLON core_type
    {()}
| COLON core_type COLONGREATER core_type
    {()}
| COLONGREATER core_type
    {()}
| COLON error
    {()}
| COLONGREATER error
    {()}

pattern:
| simple_pattern
    {()}
| pattern AS val_ident
    {()}
| pattern_comma_list %prec below_COMMA
    {()}
| constr_longident pattern %prec prec_constr_appl
    {()}
| name_tag pattern %prec prec_constr_appl
    {()}
| pattern COLONCOLON pattern
    {()}
| LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
    {()}
| pattern BAR pattern
    {()}

simple_pattern:
| val_ident %prec below_EQUAL
    {()}
| UNDERSCORE
    {()}
| signed_constant
    {()}
| CHAR DOTDOT CHAR
    {()}
| constr_longident
    {()}
| name_tag
    {()}
| SHARP type_longident
    {()}
| LBRACE lbl_pattern_list opt_semi RBRACE
    {()}
| LBRACE lbl_pattern_list opt_semi error
    {()}
| LBRACKET pattern_semi_list opt_semi RBRACKET
    {()}
| LBRACKET pattern_semi_list opt_semi error
    {()}
| LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
    {()}
| LBRACKETBAR BARRBRACKET
    {()}
| LBRACKETBAR pattern_semi_list opt_semi error
    {()}
| LPAREN pattern RPAREN
    {()}
| LPAREN pattern error
    {()}
| LPAREN pattern COLON core_type RPAREN
    {()}
| LPAREN pattern COLON core_type error
    {()}

pattern_comma_list:
| pattern_comma_list COMMA pattern
    {()}
| pattern COMMA pattern
    {()}

pattern_semi_list:
| pattern
    {()}
| pattern_semi_list SEMI pattern
    {()}

lbl_pattern_list:
| label_longident EQUAL pattern
    {()}
| lbl_pattern_list SEMI label_longident EQUAL pattern
    {()}

primitive_declaration:
| STRING
    {()}
| STRING primitive_declaration
    {()}

type_declarations:
| type_declaration
    {()}
| type_declarations AND type_declaration
    {()}

type_declaration:
| type_parameters LIDENT type_kind constraints
    {()}

constraints:
| constraints CONSTRAINT constrain
    {()}
| 
    {()}

type_kind:
| 
    {()}
| EQUAL core_type
    {()}
| EQUAL constructor_declarations
    {()}
| EQUAL PRIVATE constructor_declarations
    {()}
| EQUAL private_flag BAR constructor_declarations
    {()}
| EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
    {()}
| EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
    {()}
| EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
    {()}
| EQUAL PRIVATE core_type
    {()}

type_parameters:
| 
    {()}
| type_parameter
    {()}
| LPAREN type_parameter_list RPAREN
    {()}

type_parameter:
| type_variance QUOTE ident
    {()}

type_variance:
| 
    {()}
| PLUS
    {()}
| MINUS
    {()}

type_parameter_list:
| type_parameter
    {()}
| type_parameter_list COMMA type_parameter
    {()}

constructor_declarations:
| constructor_declaration
    {()}
| constructor_declarations BAR constructor_declaration
    {()}

constructor_declaration:
| constr_ident constructor_arguments
    {()}

constructor_arguments:
| 
    {()}
| OF core_type_list
    {()}

label_declarations:
| label_declaration
    {()}
| label_declarations SEMI label_declaration
    {()}

label_declaration:
| mutable_flag label COLON poly_type
    {()}

with_constraints:
| with_constraint
    {()}
| with_constraints AND with_constraint
    {()}

with_constraint:
| TYPE type_parameters label_longident with_type_binder core_type constraints
    {()}
| MODULE mod_longident EQUAL mod_ext_longident
    {()}

with_type_binder:
| EQUAL
    {()}
| EQUAL PRIVATE
    {()}

typevar_list:
| QUOTE ident
    {()}
| typevar_list QUOTE ident
    {()}

poly_type:
| core_type
    {()}
| typevar_list DOT core_type
    {()}

core_type:
| core_type2
    {()}
| core_type2 AS QUOTE ident
    {()}

core_type2:
| simple_core_type_or_tuple
    {()}
| QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
    {()}
| OPTLABEL core_type2 MINUSGREATER core_type2
    {()}
| LIDENT COLON core_type2 MINUSGREATER core_type2
    {()}
| core_type2 MINUSGREATER core_type2
    {()}

simple_core_type:
| simple_core_type2 %prec below_SHARP
    {()}
| LPAREN core_type_comma_list RPAREN %prec below_SHARP
    {()}

simple_core_type2:
| QUOTE ident
    {()}
| UNDERSCORE
    {()}
| type_longident
    {()}
| simple_core_type2 type_longident
    {()}
| LPAREN core_type_comma_list RPAREN type_longident
    {()}
| LESS meth_list GREATER
    {()}
| LESS GREATER
    {()}
| SHARP class_longident opt_present
    {()}
| simple_core_type2 SHARP class_longident opt_present
    {()}
| LPAREN core_type_comma_list RPAREN SHARP class_longident opt_present
    {()}
| LBRACKET tag_field RBRACKET
    {()}
| LBRACKET BAR row_field_list RBRACKET
    {()}
| LBRACKET row_field BAR row_field_list RBRACKET
    {()}
| LBRACKETGREATER opt_bar row_field_list RBRACKET
    {()}
| LBRACKETGREATER RBRACKET
    {()}
| LBRACKETLESS opt_bar row_field_list RBRACKET
    {()}
| LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
    {()}

row_field_list:
| row_field
    {()}
| row_field_list BAR row_field
    {()}

row_field:
| tag_field
    {()}
| simple_core_type2
    {()}

tag_field:
| name_tag OF opt_ampersand amper_type_list
    {()}
| name_tag
    {()}

opt_ampersand:
| AMPERSAND
    {()}
| 
    {()}

amper_type_list:
| core_type
    {()}
| amper_type_list AMPERSAND core_type
    {()}

opt_present:
| LBRACKETGREATER name_tag_list RBRACKET
    {()}
| 
    {()}

name_tag_list:
| name_tag
    {()}
| name_tag_list name_tag
    {()}

simple_core_type_or_tuple:
| simple_core_type
    {()}
| simple_core_type STAR core_type_list
    {()}

core_type_comma_list:
| core_type
    {()}
| core_type_comma_list COMMA core_type
    {()}

core_type_list:
| simple_core_type
    {()}
| core_type_list STAR simple_core_type
    {()}

meth_list:
| field SEMI meth_list
    {()}
| field opt_semi
    {()}
| DOTDOT
    {()}

field:
| label COLON poly_type
    {()}

label:
| LIDENT
    {()}

constant:
| INT
    {()}
| CHAR
    {()}
| STRING
    {()}
| FLOAT
    {()}
| INT32
    {()}
| INT64
    {()}
| NATIVEINT
    {()}

signed_constant:
| constant
    {()}
| MINUS INT
    {()}
| MINUS FLOAT
    {()}
| MINUS INT32
    {()}
| MINUS INT64
    {()}
| MINUS NATIVEINT
    {()}

ident:
| UIDENT
    {()}
| LIDENT
    {()}

val_ident:
| LIDENT
    {()}
| LPAREN operator RPAREN
    {()}

operator:
| PREFIXOP
    {()}
| INFIXOP0
    {()}
| INFIXOP1
    {()}
| INFIXOP2
    {()}
| INFIXOP3
    {()}
| INFIXOP4
    {()}
| PLUS
    {()}
| MINUS
    {()}
| MINUSDOT
    {()}
| STAR
    {()}
| EQUAL
    {()}
| LESS
    {()}
| GREATER
    {()}
| OR
    {()}
| BARBAR
    {()}
| AMPERSAND
    {()}
| AMPERAMPER
    {()}
| COLONEQUAL
    {()}

constr_ident:
| UIDENT
    {()}
| LPAREN RPAREN
    {()}
| COLONCOLON
    {()}
| FALSE
    {()}
| TRUE
    {()}

val_longident:
| val_ident
    {()}
| mod_longident DOT val_ident
    {()}

constr_longident:
| mod_longident %prec below_DOT
    {()}
| LBRACKET RBRACKET
    {()}
| LPAREN RPAREN
    {()}
| FALSE
    {()}
| TRUE
    {()}

label_longident:
| LIDENT
    {()}
| mod_longident DOT LIDENT
    {()}

type_longident:
| LIDENT
    {()}
| mod_ext_longident DOT LIDENT
    {()}

mod_longident:
| UIDENT
    {()}
| mod_longident DOT UIDENT
    {()}

mod_ext_longident:
| UIDENT
    {()}
| mod_ext_longident DOT UIDENT
    {()}
| mod_ext_longident LPAREN mod_ext_longident RPAREN
    {()}

mty_longident:
| ident
    {()}
| mod_ext_longident DOT ident
    {()}

clty_longident:
| LIDENT
    {()}
| mod_ext_longident DOT LIDENT
    {()}

class_longident:
| LIDENT
    {()}
| mod_longident DOT LIDENT
    {()}

toplevel_directive:
| SHARP ident
    {()}
| SHARP ident STRING
    {()}
| SHARP ident INT
    {()}
| SHARP ident val_longident
    {()}
| SHARP ident FALSE
    {()}
| SHARP ident TRUE
    {()}

name_tag:
| BACKQUOTE ident
    {()}

rec_flag:
| 
    {()}
| REC
    {()}

direction_flag:
| TO
    {()}
| DOWNTO
    {()}

private_flag:
| 
    {()}
| PRIVATE
    {()}

mutable_flag:
| 
    {()}
| MUTABLE
    {()}

virtual_flag:
| 
    {()}
| VIRTUAL
    {()}

opt_bar:
| 
    {()}
| BAR
    {()}

opt_semi:
| 
    {()}
| SEMI
    {()}

subtractive:
| MINUS
    {()}
| MINUSDOT
    {()}

%%
