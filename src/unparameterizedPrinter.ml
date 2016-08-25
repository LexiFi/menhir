open Positions
open Syntax
open Stretch
open UnparameterizedSyntax
open Settings

(* When the original grammar is split over several files, it may be IMPOSSIBLE
   to print it out into a single file, as this will introduce a total ordering
   (between rules, between priority declarations, between %on_error_reduce
   declarations) that did not exist originally. We currently do not warn about
   this problem. Nobody has ever complained about it. *)

let print_preludes f g =
  List.iter (fun prelude ->
    Printf.fprintf f "%%{%s%%}\n" prelude.stretch_raw_content
  ) g.preludes

let print_start_symbols b g =
  StringSet.iter (fun symbol ->
    Printf.fprintf b "%%start %s\n" (Misc.normalize symbol)
  ) g.start_symbols

let rec insert_in_partitions item m = function
  | [] ->
      [ (m, [ item ]) ]

  | (m', items) :: partitions when Error.same_input_file m m' ->
      (m', item :: items) :: partitions

  | t :: partitions ->
      t :: (insert_in_partitions item m partitions)

let insert (undefined, partitions) = function
  | (item, UndefinedPrecedence) ->
      ((item, 0) :: undefined, partitions)

  | (item, PrecedenceLevel (m, v, _, _)) ->
      (undefined, insert_in_partitions (item, v) m partitions)

let print_ocamltype ocamltype =
  Printf.sprintf " <%s>" (
    match ocamltype with
    | Declared stretch ->
        stretch.stretch_raw_content
    | Inferred t ->
        t
    )

let print_parameter f stretch =
  Printf.fprintf f "%%parameter<%s>\n" stretch.stretch_raw_content

let print_assoc = function
  | LeftAssoc ->
      Printf.sprintf "%%left"
  | RightAssoc ->
      Printf.sprintf "%%right"
  | NonAssoc ->
      Printf.sprintf "%%nonassoc"
  | UndefinedAssoc ->
      ""

let print_tokens mode b g =
  (* Sort tokens wrt precedence. *)
  let undefined, partition_tokens =
    StringMap.fold (fun token prop acu ->
      insert acu (token, prop.tk_precedence)
    ) g.tokens ([], [])
  in
  let ordered_tokens =
    List.fold_left (fun acu (_, ms) ->
      acu @ List.sort (fun (_, v) (_, v') -> compare v v') ms
    ) undefined partition_tokens
  in
  List.iter (fun (token, _) ->
    let prop = StringMap.find token g.tokens in
    if prop.tk_is_declared then
      Printf.fprintf b "%%token%s %s\n"
        begin match mode with
        | PrintNormal
        | PrintUnitActions ->
            Misc.o2s prop.tk_ocamltype print_ocamltype
        | PrintUnitActionsUnitTokens ->
            "" (* omitted ocamltype after %token means <unit> *)
        end
        token
  ) ordered_tokens;

  ignore (List.fold_left
            (fun last_prop (token, v) ->
               let prop = StringMap.find token g.tokens in
                 match last_prop with

                   | None ->
                       if prop.tk_associativity = UndefinedAssoc then
                         None
                       else (
                         Printf.fprintf b "%s %s "
                           (print_assoc prop.tk_associativity) token;
                         Some v)

                   | Some v' when v <> v' ->
                       if prop.tk_associativity = UndefinedAssoc then
                         None
                       else (
                         Printf.fprintf b "\n%s %s "
                           (print_assoc prop.tk_associativity) token;
                         Some v)

                   | Some _ ->
                       Printf.fprintf b "%s " token;
                       last_prop

            ) None ordered_tokens);
  Printf.fprintf b "\n"

let print_types mode b g =
  StringMap.iter (fun symbol ty ->
    Printf.fprintf b "%%type%s %s\n"
      begin match mode with
      | PrintNormal ->
          print_ocamltype ty
      | PrintUnitActions
      | PrintUnitActionsUnitTokens ->
          " <unit>"
      end
      (Misc.normalize symbol)
  ) g.types

let binding mode id =
  match mode with
  | PrintNormal ->
      id ^ " = "
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ""

let string_of_producer mode (symbol, ido) =
  binding mode ido ^ (Misc.normalize symbol)

let print_branch mode f branch =
  Printf.fprintf f "%s%s\n    {"
    (String.concat " " (List.map (string_of_producer mode) branch.producers))
    (Misc.o2s branch.branch_prec_annotation (fun x -> " %prec "^x.value));
  begin match mode with
  | PrintNormal ->
      Action.print f branch.action
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      () (* Printing a pair of empty braces is fine. *)
  end;
  Printf.fprintf f "}\n"

let print_postludes b g =
  List.iter (fun stretch -> Printf.fprintf b "%s\n" stretch.stretch_raw_content) g.postludes

(* Because the resolution of reduce/reduce conflicts is implicitly dictated by
   the order in which productions appear in the grammar, the printer should be
   careful to preserve this order. *)

(* 2016/08/25: As noted above, when two productions originate in different files,
   we have a problem. We MUST print them in some order, even though they should
   be incomparable. In that case, we use the order in which the source files are
   specified on the command line. However, this behavior is undocumented, and
   should not be exploited. (In previous versions of Menhir, the function passed
   to [List.sort] was not transitive, so it did not make any sense!) *)

let compare_pairs compare1 compare2 (x1, x2) (y1, y2) =
  let c = compare1 x1 y1 in
  if c <> 0 then c
  else compare2 x2 y2

let branches_order r r' =
  let branch_order b b' =
    match b.branch_production_level, b'.branch_production_level with
    | ProductionLevel (m, l), ProductionLevel (m', l') ->
        compare_pairs Error.compare_input_files Pervasives.compare (m, l) (m', l')
  in
  (* TEMPORARY I don't think we need a lexicographic ordering here *)
  let rec lexical_order bs bs' =
    match bs, bs' with
      | [], [] ->
          0
      | [], _ ->
          -1
      | _, [] ->
          1
      | b :: bs, b' :: bs' ->
          match branch_order b b' with
            | 0 ->
                lexical_order bs bs'
            | x ->
                x
  in
    lexical_order r.branches r'.branches

let print_rules mode b g =
  let rules_as_list =
    StringMap.fold (fun nt r acu -> (nt, r) :: acu) g.rules []
  in
  let ordered_rules =
    List.sort (fun (_nt, r) (_nt', r') -> branches_order r r') rules_as_list
  in
  List.iter (fun (nt, r) ->
    Printf.fprintf b "\n%s:\n" (Misc.normalize nt);
    let first = ref true in
    List.iter (fun br ->
      (* Menhir accepts a leading "|", but bison does not. Let's not print it. *)
      let sep = if !first then "  " else "| " in
      first := false;
      Printf.fprintf b "%s" sep;
      print_branch mode b br
    ) r.branches
  ) ordered_rules

let print mode f g =
  List.iter (print_parameter f) g.parameters;
  begin match mode with
  | PrintNormal ->
      print_preludes f g
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ()
  end;
  print_start_symbols f g;
  print_tokens mode f g;
  print_types mode f g;
  Printf.fprintf f "%%%%\n";
  print_rules mode f g;
  Printf.fprintf f "\n%%%%\n";
  begin match mode with
  | PrintNormal ->
      print_postludes f g
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ()
  end
