%{ (* -*- tuareg -*- *)

  open AST
  open Position
  open ParserTokens
  
  let list_of_list_option o =
    match o with 
    | Some l -> l
    | None -> []

  let add_backtick s = 
    "`" ^ s ^ "`"

  let binop_name binop =
    Id(add_backtick 
        (match binop with
        | PLUS                -> "+"
        | MINUS               -> "-"
        | STAR                -> "*"
        | SLASH               -> "/"
        | EQUALQUESTION       -> "=?"
        | LANGLEEQUALQUESTION -> "<=?"
        | RANGLEEQUALQUESTION -> ">=?"
        | LANGLEQUESTION      -> "<?"
        | RANGLEQUESTION      -> ">?"
        | DOUBLEAMPERSAND     -> "&&"
        | PIPEPIPE            -> "||"
        | _                   -> failwith "not a binop (should never be reached no matter user input)"))

%}

%start<AST.t> program
%start<AST.definition> definition
%%

program:
| definitions = list(located(definition)) EOF { definitions }
| e=located(error)                            { Error.error "parsing" e.position "Syntax error." }

definition:
| val_def = value_definition          { val_def              }
| type_def = define_type              { type_def             }
| EXTERN id=located(identifier) COLON 
         s=located(type_scheme)       { DeclareExtern(id, s) }

%public identifier:
| id=LOWERCASE_ID { Id(id) }

%public %inline located(X): 
  x=X { Position.with_poss $startpos $endpos x }

%public separated_twolong_list(SEP, ELE):
 e1=ELE SEP l=separated_nonempty_list(SEP, ELE) { e1 :: l }

%public twolong_list(ELE):
 e1=ELE l=nonempty_list(ELE) { e1 :: l }

