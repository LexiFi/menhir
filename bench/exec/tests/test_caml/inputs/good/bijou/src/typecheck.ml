open Source
open SymbolTable

let fresh () =
  assert false

let bool = 
  assert false

let atom _ =
  assert false

let data _ =
  assert false

let (=?=) _ _ =
  () (* TEMPORARY *)

let lookup _ _ =
  assert false

let introduce1 _x _t _env =
  assert false

let introduce xs t env =
  List.fold_right (fun x env ->
    introduce1 x t env
  ) xs env

let lookup_function f =
  match FuncTable.def f with
  | FunDefUser def ->
      let def = open_fundef def in
      let domain = def.fun_params
      and codomain, _ = open_guarded_tuple def.fun_postcondition.post_normal in
      domain, codomain
  | FunDefBuiltin ->
      assert false

let rec check_expression env (e : expression) expected =
  match Annotation.content e with
  | EVar x ->
      expected =?= lookup env x
  | EBool _ ->
      expected =?= bool
  | EData (dc, es) ->
      let dt, params = DataconTable.def dc in
      expected =?= data dt;
      let tuple, _ = open_guarded_tuple params in
      check_expressions env es tuple
  | EFresh fb ->
      let sort, xs, e = open_fresh_binding fb in
      let env = introduce xs (atom sort) env in
      check_expression env e expected
  | ECase (e, bs) ->
      let t = fresh() in
      check_expression env e t;
      check_branches env bs t expected
  | ECall (f, e) ->
      let domain, codomain = lookup_function f in
      expected =?= codomain;
      check_expression env e domain

and check_expressions _env (_es : expression list) _tuple =
  assert false

and check_branches env bs t expected =
  List.iter (fun b ->
    check_branch env b t expected
  ) bs

and check_branch env b domain codomain =
  let p, e = open_pat_binding b in
  let env = check_pattern env p domain in
  check_expression env e codomain

and check_pattern _env _p _expected =
  assert false

