%token<string> LOWERCASE_ID
%token<string> UPPERCASE_ID
%token<string> TYPE_VARIABLE
%token<Int64.t> INT
%token<char>   CHAR
%token<string> STRING

%token         TYPE
%token         LET
%token         FUN
%token         EXTERN
%token         BACKSLASH
%token         AND
%token         AMPERSAND

%token         UNDERSCORE

%token         IF
%token         ELSE
%token         WHILE
%token         DO
%token         FOR
%token         IN
%token         TO
%token         SWITCH

%token         REF

%token         SEMICOLON

%token         COLONEQUAL
%token         DOUBLEAMPERSAND
%token         PIPEPIPE
%token         EQUALQUESTION
%token         LANGLEEQUALQUESTION
%token         RANGLEEQUALQUESTION
%token         LANGLEQUESTION
%token         RANGLEQUESTION
%token         DOT
%token         EXCLAMATION
%token         PIPE
%token         COLON
%token         EQUAL
%token         PLUS
%token         MINUS
%token         STAR
%token         SLASH
%token         ARROW
%token         COMMA

%token         LANGLE
%token         RANGLE
%token         LBRACK 
%token         RBRACK 
%token         LPAR
%token         RPAR
%token         LCBRACK
%token         RCBRACK

%token         EOF

%right UPPERCASE_ID
%left LPAR
%left COLON

%right ARROW

%left DOUBLEAMPERSAND
%left PIPEPIPE
 
%left EQUALQUESTION LANGLEEQUALQUESTION RANGLEEQUALQUESTION LANGLEQUESTION RANGLEQUESTION

%left PLUS MINUS

%left STAR SLASH
%%
