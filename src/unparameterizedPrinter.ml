open Printf
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

(* -------------------------------------------------------------------------- *)

(* Printing an OCaml type. *)

let print_ocamltype ty : string =
  Printf.sprintf " <%s>" (
    match ty with
    | Declared stretch ->
        stretch.stretch_raw_content
    | Inferred t ->
        t
    )

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions that depend on the printing mode. *)

(* [PrintNormal] is the normal mode: the result is a Menhir grammar.

   [PrintForOCamlyacc] is close to the normal mode, but attempts to
   produces ocamlyacc-compatible output. This means, in particular,
   that we cannot bind identifiers to semantic values, but must use
   [$i] instead.

   [PrintUnitActions] causes all OCaml code to be suppressed: the
   semantic actions to be replaced with unit actions, preludes and
   postludes disappear, %parameter declarations disappear. Every
   %type declaration carries the [unit] type.

   [PrintUnitActionsUnitTokens] in addition declares every token
   to carry a semantic value of type [unit].
 *)

let print_token_type mode (prop : token_properties) =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc
  | PrintUnitActions ->
      Misc.o2s prop.tk_ocamltype print_ocamltype
  | PrintUnitActionsUnitTokens ->
      "" (* omitted ocamltype after %token means <unit> *)

let print_ocamltype_or_unit mode ty =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc ->
      print_ocamltype ty
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      " <unit>"

let print_binding mode id =
  match mode with
  | PrintNormal ->
      id ^ " = "
  | PrintForOCamlyacc
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      (* need not, or must not, bind a semantic value *)
      ""

let if_normal mode f x =
  match mode with
  | PrintNormal ->
      f x
  | PrintForOCamlyacc
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ()

let if_ocaml_code_permitted mode f x =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc ->
      f x
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      (* In these modes, all OCaml code is omitted: semantic actions,
         preludes, postludes, etc. *)
      ()

let print_semantic_action f mode branch =
  let e = Action.to_il_expr branch.action in
  match mode with
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      (* In the unit-action modes, we print a pair of empty braces, which is fine. *)
      ()
  | PrintNormal ->
      Printer.print_expr f e
  | PrintForOCamlyacc ->
       (* In ocamlyacc-compatibility mode, the code must be wrapped in
          [let]-bindings whose right-hand side uses the [$i] keywords. *)
      let bindings =
        List.mapi (fun i (_symbol, id) ->
          (* Define the variable [id] as a synonym for [$(i+1)]. *)
          IL.PVar id, IL.EVar (sprintf "$%d" (i + 1))
        ) branch.producers
      in
      (* We can use a nested sequence of [let/in] definitions, as
         opposed to a single [let/and] definitions, because the
         identifiers that we bind are pairwise distinct. *)
      let e = IL.ELet (bindings, e) in
      Printer.print_expr f e

(* -------------------------------------------------------------------------- *)

(* Printing functions. *)

let print_preludes f g =
  List.iter (fun prelude ->
    fprintf f "%%{%s%%}\n" prelude.stretch_raw_content
  ) g.preludes

let print_postludes f g =
  List.iter (fun postlude ->
    fprintf f "%s\n" postlude.stretch_raw_content
  ) g.postludes

let print_start_symbols f g =
  StringSet.iter (fun symbol ->
    fprintf f "%%start %s\n" (Misc.normalize symbol)
  ) g.start_symbols

let print_parameter f stretch =
  fprintf f "%%parameter<%s>\n" stretch.stretch_raw_content

let print_parameters f g =
  List.iter (print_parameter f) g.parameters

let print_assoc = function
  | LeftAssoc ->
      Printf.sprintf "%%left"
  | RightAssoc ->
      Printf.sprintf "%%right"
  | NonAssoc ->
      Printf.sprintf "%%nonassoc"
  | UndefinedAssoc ->
      ""

let compare_pairs compare1 compare2 (x1, x2) (y1, y2) =
  let c = compare1 x1 y1 in
  if c <> 0 then c
  else compare2 x2 y2

let compare_tokens (_token, prop) (_token', prop') =
  match prop.tk_precedence, prop'.tk_precedence with
  | UndefinedPrecedence, UndefinedPrecedence ->
      0
  | UndefinedPrecedence, PrecedenceLevel _ ->
      -1
  | PrecedenceLevel _, UndefinedPrecedence ->
      1
  | PrecedenceLevel (m, v, _, _), PrecedenceLevel (m', v', _, _) ->
      compare_pairs InputFile.compare_input_files Pervasives.compare (m, v) (m', v')

let print_tokens mode f g =
  (* Print the %token declarations. *)
  StringMap.iter (fun token prop ->
    if prop.tk_is_declared then
      fprintf f "%%token%s %s\n" (print_token_type mode prop) token
  ) g.tokens;
  (* Sort the tokens wrt. precedence, and group them into levels. *)
  let levels : (string * token_properties) list list =
    Misc.levels compare_tokens (List.sort compare_tokens (
      StringMap.bindings g.tokens
    ))
  in
  (* Print the precedence declarations: %left, %right, %nonassoc. *)
  List.iter (fun level ->
    let (_token, prop) = try List.hd level with Failure _ -> assert false in
    (* Do nothing about the tokens that have no precedence. *)
    if prop.tk_precedence <> UndefinedPrecedence then begin
      fprintf f "%s" (print_assoc prop.tk_associativity);
      List.iter (fun (token, _prop) ->
        fprintf f " %s" token
      ) level;
      fprintf f "\n"
    end
  ) levels

let print_types mode f g =
  StringMap.iter (fun symbol ty ->
    fprintf f "%%type%s %s\n"
      (print_ocamltype_or_unit mode ty)
      (Misc.normalize symbol)
  ) g.types

let print_branch mode f branch =
  (* Print the producers. *)
  let sep = Misc.once "" " " in
  List.iter (fun (symbol, id) ->
    fprintf f "%s%s%s" (sep()) (print_binding mode id) (Misc.normalize symbol)
  ) branch.producers;
  (* Print the %prec annotation, if there is one. *)
  Option.iter (fun x ->
    fprintf f " %%prec %s" x.value
  ) branch.branch_prec_annotation;
  (* Newline, indentation, semantic action. *)
  fprintf f "\n    {";
  print_semantic_action f mode branch;
  fprintf f "}\n"

(* Because the resolution of reduce/reduce conflicts is implicitly dictated by
   the order in which productions appear in the grammar, the printer should be
   careful to preserve this order. *)

(* 2016/08/25: As noted above, when two productions originate in different files,
   we have a problem. We MUST print them in some order, even though they should
   be incomparable. In that case, we use the order in which the source files are
   specified on the command line. However, this behavior is undocumented, and
   should not be exploited. (In previous versions of Menhir, the function passed
   to [List.sort] was not transitive, so it did not make any sense!) *)

let compare_branch_production_levels bpl bpl' =
  match bpl, bpl' with
  | ProductionLevel (m, l), ProductionLevel (m', l') ->
      compare_pairs InputFile.compare_input_files Pervasives.compare (m, l) (m', l')

let compare_branches (b : branch) (b' : branch) =
  compare_branch_production_levels b.branch_production_level b'.branch_production_level

let compare_rules (_nt, (r : rule)) (_nt', (r' : rule)) =
  match r.branches, r'.branches with
  | [], [] ->
      0
  | [], _ ->
      -1
  | _, [] ->
      1
  | b :: _, b' :: _ ->
      (* To compare two rules, it suffices to compare their first productions. *)
      compare_branches b b'

let print_rules mode f g =
  let rules = List.sort compare_rules (StringMap.bindings g.rules) in
  List.iter (fun (nt, r) ->
    fprintf f "\n%s:\n" (Misc.normalize nt);
    (* Menhir accepts a leading "|", but bison does not. Let's not print it.
       So, we print a bar-separated list. *)
    let sep = Misc.once ("  ") ("| ") in
    List.iter (fun br ->
      fprintf f "%s" (sep());
      print_branch mode f br
    ) r.branches
  ) rules

let print_on_error_reduce_declarations f g =
  let cmp (_nt, oel) (_nt', oel') =
    compare_branch_production_levels oel oel'
  in
  let levels : (string * on_error_reduce_level) list list =
    Misc.levels cmp (List.sort cmp (
      StringMap.bindings g.on_error_reduce
    ))
  in
  List.iter (fun level ->
    fprintf f "%%on_error_reduce";
    List.iter (fun (nt, _level) ->
      fprintf f " %s" nt
    ) level;
    fprintf f "\n"
  ) levels

let print mode f g =
  if_normal mode (print_parameters f) g;
  if_ocaml_code_permitted mode (print_preludes f) g;
  print_start_symbols f g;
  print_tokens mode f g;
  print_types mode f g;
  if_normal mode (print_on_error_reduce_declarations f) g;
  fprintf f "%%%%\n";
  print_rules mode f g;
  fprintf f "\n%%%%\n";
  if_ocaml_code_permitted mode (print_postludes f) g
