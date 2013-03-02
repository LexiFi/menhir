open Source

(* ------------------------------------------------------------------------- *)

(* Display. *)

let print_primitive = function
  | PrimBoolAnd ->
      "and"
  | PrimBoolOr ->
      "or"
  | PrimBoolNot ->
      "not"
  | PrimAtomEquality ->
      "=="
  | PrimGenericSupport ->
      "free"
  | PrimGenericOuter ->
      "outer"
  | PrimGenericInner ->
      "inner"
  | PrimGenericBound ->
      "bound"
  | PrimSetEmpty ->
      "empty"
  | PrimSetMember ->
      "member"
  | PrimSetAdd ->
      "add"
  | PrimSetUnion ->
      "U"
  | PrimSetInter ->
      "inter"
  | PrimSetMinus ->
      "\\"
  | PrimSetIsEmpty ->
      "empty?"
  | PrimSetChoose ->
      "choose"
  | PrimSingletonRename ->
      "rename"

let print_callee = function
  | CUser f ->
      SymbolTable.funname f
  | CPrim p ->
      print_primitive p

(* ------------------------------------------------------------------------- *)

(* Specifications. *)

(* TEMPORARY make sure that specs are parsed at compile time or at
   startup time, but not on demand *)

(* TEMPORARY making singleton, etc. syntactic sugar for more basic
   primitive operations is nice, but leads to more complex postconditions
   (extra intermediate variables). Avoid. *)

let specification = function
  | PrimBoolAnd ->
      "accepts x, y produces z where z <-> (x and y)"
  | PrimBoolOr ->
      "accepts x, y produces z where z <-> (x or y)"
  | PrimBoolNot ->
      "accepts x produces y where y <-> not x"
  | PrimAtomEquality ->
      "accepts x, y produces b where b -> free(x) == free(y) \
                                     not b -> free(x) # free(y)"
  | PrimGenericSupport ->
      "accepts x produces s where free(s) == free(x)"
  | PrimGenericOuter ->
      "accepts x produces s where free(s) == outer(x)"
  | PrimGenericInner ->
      "accepts x produces s where free(s) == inner(x)"
  | PrimGenericBound ->
      "accepts x produces s where free(s) == bound(x)"
  | PrimSetEmpty ->
      "accepts nothing produces s where free(s) == empty"
  | PrimSetMember ->
      "accepts x, s produces b where b -> free(x) <= free(s) \
                                     not b -> free(x) # free(s)"
  | PrimSetAdd ->
      "accepts x, s produces t where free(t) == free(x) U free(s)"
  | PrimSetUnion ->
      "accepts s1, s2 produces t where free(t) == free(s1) U free(s2)"
  | PrimSetInter ->
      "accepts s1, s2 produces t where free(t) == free(s1) inter free(s2)"
  | PrimSetMinus ->
      "accepts s1, s2 produces t where free(t) == free(s1) \\ free(s2)"
  | PrimSetIsEmpty ->
      "accepts s produces b where b <-> free(s) == empty"
  | PrimSetChoose ->
      "accepts s where free(s) != empty produces x, t where free(x) U free(t) == free(s) \
                                                            free(x) # free(t)"
  | PrimSingletonRename ->
      "accepts a, b, t                                                        \
       produces u, f where free(u) <= (free(t) \\ free(a)) U free(b)          \
	                   free(t) \\ free(a) <= free(u)                      \
                           f -> free(a) # free(t)                             \
		           f -> free(b) # free(u)                             \
		           not f -> free(a) <= free(t)                        \
		           not f -> free(b) <= free(u)                        "
			 (* TEMPORARY the first two conjuncts in this spec should suffice in many situations? *)

let specification p =
  let spec : Raw.specification =
    let lexbuf =
      Lexing.from_string (specification p)
    in
    try
      Parser.specification_alone Lexer.main lexbuf
    with
    | Parser.Error ->
	Error.error2
	  (Lexing.lexeme_start_p lexbuf)
	  (Lexing.lexeme_end_p lexbuf)
	  (Printf.sprintf
	     "Syntax error in the specification of the primitive function %s."
	     (print_primitive p))
  in
  let spec : specification =
    import_specification Identifier.Map.empty spec
  in
  let xs, pre, opc = open_osp spec in
  let ys, post = open_opc opc in
  xs, pre, ys, post

let specification =
  Misc.memoize specification

let specification = function
  | CUser f ->
      let xs, pre, ys, post, _ = SymbolTable.fundef f in
      xs, pre, ys, post
  | CPrim p ->
      specification p

