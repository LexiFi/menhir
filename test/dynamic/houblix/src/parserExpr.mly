%%

%public value_definition:
| v = vdefinition { DefineValue v }

vdefinition:
| LET   id=located(identifier) 
        s=option(preceded(COLON, located(type_scheme))) 
  EQUAL e=located(expr)                                 { SimpleValue(id, s, e) }
| d = function_definitions                              { d                     }

simple_vdefinition:
| LET   id=located(identifier) 
        s=option(preceded(COLON, located(type_scheme))) 
  EQUAL e=located(nodef_expr)                           { SimpleValue(id, s, e) }
| d = simple_function_definitions                       { d                     }

function_definitions:
| FUN l=separated_nonempty_list(AND, function_definition) { RecFunctions(l) }

function_definition:
| s=option(preceded(COLON, located(type_scheme))) 
  id=located(identifier) 
  arg = located(pattern) EQUAL 
  e=located(expr)                                 { (id, s, FunctionDefinition(arg, e)) }

simple_function_definitions:
| FUN l=separated_nonempty_list(AND, simple_function_definition) { RecFunctions(l) }
simple_function_definition:
| s=option(preceded(COLON, located(type_scheme))) 
  id=located(identifier) 
  arg=located(pattern) EQUAL 
  e=located(nodef_expr)                           { (id, s, FunctionDefinition(arg, e)) }


%public expr:
| e=nodef_expr                                      { e                   }
| e1=located(nodef_expr) SEMICOLON e2=located(expr) { Sequence ([e1; e2]) }
| d=simple_vdefinition SEMICOLON e=located(expr)    { Define(d, e)        }

nodef_expr:
| e=binop_prio_0_expr                      { e                                    }
| e=control_structure                      { e                                    }
| BACKSLASH arg=located(pattern) ARROW 
            body=located(nodef_expr)       { Fun(FunctionDefinition(arg, body))   }
| e1=located(binop_prio_0_expr) COLONEQUAL 
  e2=located(binop_prio_0_expr)            { Assign(e1, e2)                       }

binop_prio_0_expr:
| e = binop_prio_1_expr                                   { e }
| e = binop(binop_prio_0_expr, prio_0, binop_prio_0_expr) { e }

binop_prio_1_expr:
| e = binop_prio_2_expr                                   { e }
| e = binop(binop_prio_1_expr, prio_1, binop_prio_1_expr) { e }

binop_prio_2_expr:
| e = apply_expr                                          { e }
| e = binop(binop_prio_2_expr, prio_2, binop_prio_2_expr) { e }

apply_expr:
| e = ref_deref_expr                      { e                                    }
| l=twolong_list(located(ref_deref_expr)) { ASTHelper.expr_of_apply_list l  }

ref_deref_expr:
| REF e=located(atomic_expr)           { Ref e                                 }
| EXCLAMATION e=located(ref_deref_expr)   { Read e                                }
| e = atomic_expr                      { e                                     }


atomic_expr:
| r = record                            { r                    }
| t = tuple                             { t                    }
| e=located(atomic_expr) DOT 
  l=located(label)                      { Field(e, l)          }
| LPAR e=expr RPAR                      { e                    }
| LPAR e=located(expr) 
       COLON 
       t=located(type_) 
  RPAR                                  { TypeAnnotation(e, t) }
| v=variable                            { v                    }
| l=located(literal)                    { Literal l            }
| const=located(constructor) 
  type_args=option(type_argument_apply) 
  args=ioption(constructor_arguments)   { Tagged(const, type_args, list_of_list_option args) }

%inline constructor_arguments:
| LPAR l=separated_nonempty_list(COMMA, located(binop_prio_0_expr)) RPAR { l }

%inline tuple:
| LPAR  l = separated_twolong_list(COMMA, located(binop_prio_0_expr)) RPAR { Tuple(l) }

%inline variable:
| id = located(identifier) 
  types=option(type_argument_apply) { Variable (id, types) }

%inline record:
| LCBRACK 
    l=separated_nonempty_list(COMMA, record_expr_member) 
  RCBRACK 
  types=option(type_argument_apply)                      { Record(l, types) }


%inline binop(E1, OP, E2):
| e1 = located(E1) b = located(OP) e2 = located(E2) { Apply(
                                                        { value=Apply(b, e1); position=join e1.position b.position }, e2) }
%inline prio_2:
(* int -> int -> int *)
| p = located(STAR)  { Variable(with_val (binop_name STAR) p, None)  }
| p = located(SLASH) { Variable(with_val (binop_name SLASH) p, None) }

%inline prio_1:
(* int -> int -> int *)
| p = located(MINUS) { Variable(with_val (binop_name MINUS) p, None) }
| p = located(PLUS)  { Variable(with_val (binop_name PLUS) p, None)  }
(* int -> int -> bool *)
| p = located(EQUALQUESTION)       { Variable(with_val (binop_name EQUALQUESTION) p, None)       }
| p = located(LANGLEEQUALQUESTION) { Variable(with_val (binop_name LANGLEEQUALQUESTION) p, None) }
| p = located(RANGLEEQUALQUESTION) { Variable(with_val (binop_name RANGLEEQUALQUESTION) p, None) }
| p = located(LANGLEQUESTION)      { Variable(with_val (binop_name LANGLEQUESTION) p, None)      }
| p = located(RANGLEQUESTION)      { Variable(with_val (binop_name RANGLEQUESTION) p, None)      }

%inline prio_0:
(* bool -> bool -> bool *)
| p = located(DOUBLEAMPERSAND)   { Variable(with_val (binop_name DOUBLEAMPERSAND) p, None)   }
| p = located(PIPEPIPE)          { Variable(with_val (binop_name PIPEPIPE) p, None)          }

%inline literal:
| c = CHAR   { LChar c   }
| s = STRING { LString s }
| i = INT    { LInt i    }


record_expr_member:
| label=located(label) EQUAL e=located(nodef_expr) { (label, e) }

pattern:
| p=atomic_pattern                                                      { p                     }
| p=located(atomic_pattern) COLON t=located(type_)                      { PTypeAnnotation(p, t) }
| LPAR  l=separated_twolong_list(COMMA, located(pattern)) RPAR          { PTuple(l)             }
| branches = separated_twolong_list(PIPE, located(atomic_pattern))      { POr(branches)         }
| branches = separated_twolong_list(AMPERSAND, located(atomic_pattern)) { PAnd(branches)        }

record_pattern:
| l=located(label) EQUAL p=located(pattern) { (l, p) }

atomic_pattern:
(* Not in the grammar as is. Permited by the tuple rule. *)
| LPAR p = pattern RPAR    { p             }
| c=located(constructor)
  targs=option(type_argument_apply)
  l=option(
      delimited(
        LPAR, 
        separated_nonempty_list(COMMA, located(pattern)), 
        RPAR))                                            { PTaggedValue(c, targs, list_of_list_option l) }
| UNDERSCORE                                              { PWildcard                                     }
| id = located(identifier)                                { PVariable id                                  }
| lit = located(literal)                                  { PLiteral lit                                  }
| LCBRACK  
    l=separated_nonempty_list(COMMA, record_pattern) 
  RCBRACK 
  t = option(type_argument_apply)                         { PRecord(l, t)                                 }




control_structure:
| IF LPAR cond=located(expr) RPAR 
  LCBRACK body1=located(expr) RCBRACK 
  ELSE 
  LCBRACK body2=located(expr) RCBRACK    { IfThenElse(cond, body1, body2) }
| FOR     id=located(identifier) IN 
  LPAR    e1=located(expr) 
    TO    e2=located(expr) 
  RPAR
  LCBRACK body=located(expr) RCBRACK     { For(id, e1, e2, body)                 }

| WHILE LPAR cond=located(expr) RPAR 
  LCBRACK    body=located(expr) RCBRACK  { While(cond, body)                     }
| DO LCBRACK body=located(expr) RCBRACK 
  WHILE LPAR cond=located(expr) RPAR     { Sequence([ body 
                                                    ; { value=While(cond, body)
                                                      ; position=body.position }
                                                    ])                           }
| SWITCH LPAR cond=located(expr) RPAR 
  LCBRACK     cases=switch_cases RCBRACK { Case(cond, cases)                     }

switch_cases:
| option(PIPE) cases=separated_nonempty_list(PIPE, located(switch_branch)) { cases }

switch_branch:
| p=located(pattern) ARROW e=located(expr) { Branch(p, e) }