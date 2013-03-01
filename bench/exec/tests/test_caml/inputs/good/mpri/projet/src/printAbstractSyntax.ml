(* Pretty-printer for abstract syntax *)

open Format
open AbstractSyntax

let print_type_variable pp v = fprintf pp "'%s" v

let print_typ = Type.print print_type_variable

let rec print_expr_0 pp = function
  | EVar x -> fprintf pp "%s" x
  | EConApp(cstr, []) -> fprintf pp "%s" cstr
  | a -> fprintf pp "@[<hov 1>(%a)@]" print_expr a

and print_expr_1 pp = function
  | EApp(_, _) as a ->
      fprintf pp "@[<hov 2>";
      let rec print_app pp = function
      | EApp(a, b) ->
          fprintf pp "%a@ %a" print_app a print_expr_0 b
      | a ->
          print_expr_0 pp a in
      print_app pp a;
      fprintf pp "@]"
  | EConApp(cstr, a :: al) ->
      fprintf pp "%s@[<hov 1>(%a" cstr print_expr a;
      List.iter (fun a' -> fprintf pp ",@ %a" print_expr a') al;
      fprintf pp ")@]"
  | a ->
      print_expr_0 pp a
  
and print_expr pp = function
  | EFun(x, a) ->
      fprintf pp "@[<hov 1>fun %s ->@ %a@]" x print_expr a
  | ERecFun(f, x, a) ->
      fprintf pp "@[<hov 1>let rec %s %s =@ %a@ in %s@]" f x print_expr a f
  | ELet(f, ERecFun(f', x, a), b) when f = f' ->
      fprintf pp "@[<hov 0>@[<hv 2>let rec %s %s =@ %a in@]@ %a@]"
        f x print_expr a print_expr b
  | ELet(x, a, b) ->
      fprintf pp "@[<hov 0>@[<hv 2>let %s =@ %a@] in@ %a@]"
        x print_expr a print_expr b
  | EMatch(a, bl) ->
      fprintf pp "@[<hv 0>match %a with" print_expr a;
      List.iter (print_branch pp) bl;
      fprintf pp "@ end@]"
  | ERaise a ->
      fprintf pp "raise %a" print_expr_0 a
  | ETryWith(a, x, b) ->
      fprintf pp "@[<hov 0>try %a@ with %s ->@ %a@ end@]"
              print_expr a x print_expr b
  | EExists(vars, a) ->
      fprintf pp "@[<hov 2>exists";
      List.iter (fun v -> fprintf pp " '%s" v) vars;
      fprintf pp ".@ %a@]" print_expr a
  | ETypeAnnotation(a, ty) ->
      fprintf pp "(@[<hov 2>%a@ : %a@])" print_expr a print_typ ty
  | a ->
      print_expr_1 pp a

and print_branch pp (Branch(pat, expr)) =
  fprintf pp "@ @[<hov 4>| %a ->@ %a@]" print_pat pat print_expr expr

and print_pat pp (PConApp(cstr, args)) =
  match args with
  | [] ->
      fprintf pp "%s" cstr
  | a1 :: al ->
      fprintf pp "%s(%s" cstr a1;
      List.iter (fun a -> fprintf pp ", %s" a) al;
      fprintf pp ")"

let print_data_constructor_definition pp (DefCon(cstr, args)) =
  match args with
  | [] ->
      fprintf pp "%s" cstr
  | t1 :: tl ->
      fprintf pp "%s of @[<hov 0>%a" cstr print_typ t1;
      List.iter (fun t -> fprintf pp " *@ %a" print_typ t) tl;
      fprintf pp "@]"

let print_data_type_definition pp (DefDataType(vars, tc, dcl)) =
  fprintf pp "@[<v 2>type ";
  begin match vars with
  | [] -> ()
  | [v1] -> fprintf pp "'%s " v1
  | v1 :: vl ->
      fprintf pp "('%s" v1;
      List.iter (fun v -> fprintf pp ", %s" v) vl;
      fprintf pp ") "
  end;
  fprintf pp "%s =" tc;
  List.iter
    (fun dc -> fprintf pp "@ | %a" print_data_constructor_definition dc)
    dcl;
  fprintf pp "@]@ @ "

let print_program (Program(decls, body)) =
  printf "@[<v 0>";
  List.iter (print_data_type_definition std_formatter) decls;
  printf "program@ @ ";
  printf "%a@]@.\n" print_expr body
