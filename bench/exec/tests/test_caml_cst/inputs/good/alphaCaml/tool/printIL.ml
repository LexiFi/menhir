open Pprint
open IL

let (++) x y =
  x ^^ softline ^^ y

let bar =
  text "| "

let optional_bar =
  ifflat empty bar

let smart_bar () =
  let first = ref true in
  fun () ->
    if !first then begin
      first := false;
      optional_bar
    end
    else
      bar

let skip =
  linebreak ^^ line

let arrow =
  text " ->" ^^ line

let equals =
  text " =" ^^ line

let kin =
  line ^^ text "in"

let star =
  text " *" ^^ line

let comma =
  comma ^^ line

let semi =
  semi ^^ line

let tyvar v =
  squote ^^ text v

let block left right d =
  group (
    nest 2 (left ^^ d) ^^ right
  )

let delimited_block opening left right closing d =
  text opening ^^ block left right d ^^ text closing

let beginend =
  block
    (ifflat lparen (text "begin" ^^ hardline))
    (ifflat rparen (hardline ^^ text "end"))

let structend =
  delimited_block "struct" skip skip "end"

let sigend =
  delimited_block "sig" skip skip "end"

let self f x =
  space ^^ parens (f x)

let objectend f x =
  delimited_block "object" (optional (self f) x ^^ skip) skip "end"

let funBlock d =
  text "fun" ^^ block line empty d

let blocksep sep g ds =
  block linebreak linebreak (sepmap sep g ds)

let tupled g ds =
  parens (blocksep comma g ds)

let sqtupled g ds =
  brackets (blocksep comma g ds)

let record_header h =
  h ++ text "with" ++ empty

let semiBraces header g ds =
  braces (
    block line line (
      optional record_header header ^^ sepmap semi g ds
    )
  )

let bind k x f e =
  nest 2 (group (
    text k ++ x ^^ equals ^^ f e
  ))

let bind1 k x f e =
  bind k (text x) f e

let annotate f x g y =
  group (
    f x ^^ colon ++ g y
  )

let annotateo f x g y =
  match y with
  | None ->
      f x
  | Some y ->
      annotate f x g y

let declare k x f e =
  nest 2 (group (
    text k ++ annotate text x f e
  ))

let tyapp tycon g typarams =
  match typarams with
  | [] ->
      tycon
  | [ typaram ] ->
      group (g typaram ++ tycon)
  | _ ->
      group (tupled g typarams ++ tycon)

let ctyapp ccon g typarams =
  match typarams with
  | [] ->
      ccon
  | _ ->
      group (sqtupled g typarams ++ ccon)

let rec path = function
  | PathVar v ->
      text v
  | PathDot (p, v) ->
      path p ^^ dot ^^ text v

let rec expr0 = function
  | EWildcard ->
      text "_"
  | EVar p
  | EData (p, []) ->
      path p
  | ERecord fes ->
      semiBraces None binding fes
  | ETuple es ->
      tuple es
  | ELiteral lit ->
      parens (string lit)
  | EMatch (e, bs) ->
      beginend (
        text "match " ^^ expr e ^^ text " with" ^^ line ^^
        branches bs
      )
  | ETry (e, bs) ->
      beginend (
        text "try " ^^ expr e ^^ text " with" ^^ line ^^
        branches bs
      )
  | ERecordAccess (e, field) ->
      expr0 e ^^ dot ^^ text field
  | EMethodCall (e, field) ->
      expr0 e ^^ sharp ^^ text field
  | ERecordUpdate (e, []) ->
      expr0 e
  | ERecordUpdate (e, fes) ->
      semiBraces (Some (expr e)) binding fes
  | ETypeConstraint (e, t) ->
      parens (annotate expr e typ t)
  | ENew p ->
      text "new" ++ path p
  | EFun _
  | EData (_, _ :: _)
  | EApp _
  | EInfixApp _
  | ELet _
  | EIfThenElse _
  | ERaise _
    as e ->
      beginend (expr e)

and tuple es =
  tupled expr2 es

and expr1 = function
  | EData (p, []) ->
      path p
  | EData (p, es) ->
      path p ++ tuple es
  | EApp (e1, e2) ->
      expr1 e1 ++ expr0 e2
  | ERaise e ->
      text "raise" ++ expr0 e
  | e ->
      expr0 e

and expr2 = function
  | EInfixApp (e1, op, e2) ->
      group (expr1 e1 ^^ space ^^ string op ^^ line ^^ expr1 e2)
  | e ->
      expr1 e

and expr3 = function
  | EFun (ps, e) ->
      funBlock (
	patterns0 ps ^^ arrow ^^
	expr e
      )
  | ELet (p, e1, e2) ->
      text "let " ^^ pattern p ^^ block equals kin (expr e1) ^^ line ^^
      expr3 e2
  | EIfThenElse (e0, e1, e2) ->
      group (
        text "if" ^^ block line line (expr e0) ^^
        text "then" ^^ block line line (expr e1) ^^
        text "else" ^^ block line empty (expr3 e2)
      )
  | e ->
      expr2 e

and patterns0 ps =
  sepmap softline expr0 ps

and expr e =
  expr3 e

and pattern p =
  expr p

and binding (field, e) =
  group (text field ^^ equals ^^ expr1 e)

and branches bs =
  sepmap line (branch (smart_bar())) bs

and branch bar b =
  bar() ^^ group (
    pattern b.branchpat ^^ nest 4 (arrow ^^ 
      expr b.branchbody
    )
  )

and structure_item = function
  | StructValDef (x, EFun (ps, e)) ->
      bind "let" (text x ++ patterns0 ps) expr e
  | StructValDef (x, e) ->
      bind1 "let" x expr e
  | StructTypeDefs tds ->
      typedefs tds
  | StructModuleDef (x, e) ->
      bind1 "module" x modulexpr e
  | StructModuleTypeDef (x, e) ->
      bind1 "module type" x moduletypexpr e
  | StructClassDef (v, params, x, e) ->
      bind (virtuality "class" v) (ctyapp (text x) tyvar params) classexpr e
  | StructInclude e ->
      text "include " ^^ modulexpr e
  | StructOpen p ->
      text "open " ^^ path p
  | StructComment c ->
      group (text "(* " ^^ (nest 3 (words c ^^ text " *)")))

and virtuality k = function
  | Virtual ->
      k ^ " virtual"
  | NonVirtual ->
      k

and privacy k = function
  | Private ->
      k ^ " private"
  | Public ->
      k
and signature_item = function
  | SigValDecl (x, ts) ->
      declare "val" x typescheme ts
  | SigTypeDecls tds ->
      typedefs tds
  | SigModuleDecl (x, e) ->
      declare "module" x moduletypexpr e
  | SigModuleTypeDecl (x, e) ->
      declare "module type" x moduletypexpr e
  | SigInclude e ->
      text "include " ^^ moduletypexpr e
  | SigComment c ->
      string c
  | SigOpen p ->
      text "open " ^^ path p

and typedef first td =
  text (if first then "type" else "and") ++
  tyapp (text td.typename) tyvar td.typeparams ^^
  optional tyrhs td.typerhs

and typedefs = function
  | [] ->
      empty
  | [ td ] ->
      typedef true td
  | td :: tds ->
      typedef true td ^^
      skip ^^
      sepmap skip (typedef false) tds

and tyrhs rhs =
  nest 2 (
    equals ^^
    match rhs with
    | ADT.Sum summands ->
	sepmap line (fun (data, ts) ->
	  group (
	    text "| " ^^ text data ^^
	    match ts with
	    | [] ->
		empty
	    | _ ->
		text " of " ^^ typ (TypTuple ts)
	  )
	) summands
    | ADT.Record conjuncts ->
	semiBraces None simple_binding conjuncts
    | ADT.Tuple ts ->
	typ (TypTuple ts)
  )

and simple_binding (x, t) =
  annotate text x typ t

and self_binding (x, ot) =
  match ot with
  | Some t ->
      simple_binding (x, t)
  | None ->
      text x

and structure_items items =
  sepmap skip structure_item items

and modulexpr0 = function
  | MEPath p ->
      path p
  | MEStruct items ->
      structend (structure_items items)
  | MEFunctor _
  | MEApply _
    as e ->
      parens (modulexpr e)

and modulexpr1 = function
  | MEApply (e1, e2) ->
      modulexpr1 e1 ^^ parens (modulexpr e2)
  | e ->
      modulexpr0 e

and modulexpr2 = function
  | MEFunctor (x, t, e) ->
      group (
        text "functor" ++
	parens (annotate text x moduletypexpr t) ^^ arrow ^^
        modulexpr2 e
      )
  | e ->
      modulexpr1 e

and modulexpr e =
  modulexpr2 e

and moduletypexpr = function
  | MTEPath p ->
      path p
  | MTESig items ->
      sigend (
        sepmap skip signature_item items
      )
  | MTEWith (e, td) ->
      group (nest 2 (
        moduletypexpr e ++
	group (text "with " ^^ typedef true td)
      ))

and typ0 = function
  | TypLiteral lit ->
      parens (string lit)
  | TypVar v ->
      tyvar v
  | TypApp (p, ts) ->
      tyapp (path p) typ ts
  | t ->
      parens (typ t)

and typ1 = function
  | TypTuple [] ->
      text "unit"
  | TypTuple [ t ] ->
      typ1 t
  | TypTuple ts ->
      group (sepmap star typ0 ts)
  | t ->
      typ0 t

and typ2 = function
  | TypArrow (t1, t2) ->
      group (typ1 t1 ^^ arrow ^^ typ2 t2)
  | t ->
      typ1 t

and typ t =
  typ2 t

and quantifiers = function
  | [] ->
      empty
  | qs ->
      sepmap softline tyvar qs ++ dot ^^ line

and typescheme ts =
  group (
    quantifiers ts.quantifiers ^^
    typ ts.body
  )
      
and classexpr0 = function
  | CPath p ->
      path p
  | CObject (selfo, items) ->
      objectend self_binding selfo (
        sepmap skip class_item items
      )
  | CFun _
  | CApp _
    as e ->
      parens (classexpr e)

and classexpr1 = function
  | CApp (e, es) ->
      classexpr1 e ++ sepmap softline expr0 es
  | e ->
      classexpr0 e

and classexpr2 = function
  | CFun (xs, e) ->
      funBlock (
	sepmap softline text xs ^^ arrow ^^
	classexpr e
      )
  | e ->
      classexpr1 e

and classexpr e =
  classexpr2 e

and class_item = function
  | CIMethod (_, _, _, None, None) ->
      assert false
  | CIMethod (p, v, x, Some ts, None) ->
      text (virtuality (privacy "method" p) v) ++
      annotate text x typescheme ts
  | CIMethod (p, v, x, tso, Some e) ->
      bind
	(virtuality (privacy "method" p) v)
	(annotateo text x typescheme tso)
	expr e
  | CIInherit e ->
      text "inherit" ++ classexpr e
