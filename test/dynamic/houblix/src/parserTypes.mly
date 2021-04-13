%%

%public type_argument_apply:
(* according to spec, this should be 
| LANGLE args = separated_nonempty_list(COMMA, located(type_)) RANGLE { args }
Is not because of test 47-instanciation.bad *)
| LANGLE args = separated_list(COMMA, located(type_)) RANGLE { args }

%public type_scheme:
| l = option(delimited(LBRACK, nonempty_list(located(type_variable)), RBRACK)) 
  t = located(type_)                                                           { ForallTy(list_of_list_option l, t) }

%public %inline constructor:
| id = UPPERCASE_ID { KId id }

%public define_type:
| TYPE id=located(type_constructor) 
  args=option(type_argument_declaration) EQUAL 
  t=type_definition                            { DefineType(id, list_of_list_option args, t) }

%public label:
| id=LOWERCASE_ID { LId id }

%public type_:
| t = simple_type     { t               }
| t1 = located(type_) 
  ARROW 
  t2 = located(type_) { TyArrow(t1, t2) }

simple_type:
| t = very_simple_type                                        { t          }
| l = separated_twolong_list(STAR, located(very_simple_type)) { TyTuple(l) }
 
very_simple_type:
| LPAR t = type_ RPAR              { t                                    }
| var = type_variable              { TyVar var                            }
| con = type_constructor 
  args=option(type_argument_apply) { TyCon(con, list_of_list_option args) }
 
type_argument_declaration:
| LANGLE args = separated_nonempty_list(COMMA, located(type_variable)) RANGLE { args }

type_definition:
| option(PIPE) l=separated_nonempty_list(PIPE, sum_type_constructor_definition) { DefineSumType(l)   }
| LCBRACK l=separated_nonempty_list(COMMA, record_def) RCBRACK                  { DefineRecordType l }

record_def:
| label=located(label) COLON 
  ty=located(type_)          { (label, ty) }

sum_type_constructor_definition:
| c=located(constructor) 
  args=option(sum_type_constructor_arg_list) { (c, list_of_list_option args) }

sum_type_constructor_arg_list:
| LPAR l = separated_nonempty_list(COMMA, located(type_)) RPAR { l }

type_constructor:
| id=LOWERCASE_ID { TCon(id) }

type_variable:
| v = TYPE_VARIABLE { TId v }
