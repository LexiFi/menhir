open Format
open AbstractSyntax

let rec lift f = function
  | TVar x ->
      f x
  | TArrow (t1, t2) ->
      TArrow (lift f t1, lift f t2)
  | TConApp (dc, ts) ->
      TConApp (dc, List.map (lift f) ts)

let print pv pp ty =

  let rec print_typ_0 pp = function
    | TVar v ->
        pv pp v
    | TConApp(cstr, []) ->
        fprintf pp "%s" cstr
    | TConApp(cstr, [t1]) ->
        fprintf pp "%a %s" print_typ_0 t1 cstr
    | TConApp(cstr, t1 :: tl) ->
        fprintf pp "(%a" print_typ t1;
        List.iter (fun t -> fprintf pp ", %a" print_typ t) tl;
        fprintf pp ") %s" cstr
    | t ->
        fprintf pp "@[<hov 2>%a@]" print_typ t

  and print_typ pp = function
    | TArrow(t1, t2) ->
        fprintf pp "@[<hov 0>%a ->@ %a@]" print_typ_0 t1 print_typ t2
    | t ->
        print_typ_0 pp t

  in print_typ pp ty
