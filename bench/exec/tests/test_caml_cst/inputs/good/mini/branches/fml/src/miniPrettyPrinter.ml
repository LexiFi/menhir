(* $Id$ *)

(** This module provides a pretty-printer for the Mini language. *)

open Sig
open Positions
open Format
open Misc
open PrettyPrinter
open MiniPst

let print_program_task = "print-program"

let ppf = ref std_formatter

let rec print_separated_list ?cut sep printer = function
    [] -> ()
  | [ a ] -> 
      printer a
  | a :: q -> 
      printer a; 
      if cut <> None then pp_print_cut !ppf ();
      pp_print_string !ppf sep; 
      print_separated_list ?cut:cut sep printer q

      
let symbol_of_kvariance = function
  | KInVariant -> "*"
  | KCoVariant -> "+"
  | KContraVariant -> "-"   
  | _ -> raise (Invalid_argument "symbol_of_kvariance expects a KVariance as argument")
      
      
let rec print_expression e = 
  (match e with
     | EPrimApp (_, PCharConstant c, _) -> 
        pp_print_string !ppf ("'"^ String.make 1 c ^"'")
     | EPrimApp (_, PIntegerConstant i, _) -> 
        pp_print_string !ppf (string_of_int i)
     | EPrimApp (_, PUnit, _) -> 
        pp_print_string !ppf "()"
     | EAssertFalse _ -> pp_print_string !ppf "assert false"
     | EError _ -> assert false
     | EVar (_, name) -> pp_print_string !ppf name
     
     | EDCon (_, name ) -> pp_print_string !ppf name
    (* TODO_DISPLAY
      | EDConAnnoted (_, "_Unit", annot, es) ->
         pp_print_string !ppf "()"
     | EDConAnnoted (_, "_Tuple", annot, es) ->
         pp_print_string !ppf "(";
         print_separated_list "," print_annoted_expression es;
         pp_print_string !ppf ")"
     | EDConAnnoted (_, dname, annot, []) ->
         pp_print_string !ppf "(";
         pp_print_string !ppf dname; 
         pp_print_string !ppf ")"
     | EDConAnnoted (_, dname, annot, es) -> 
         pp_print_string !ppf "(";
         pp_print_string !ppf dname; 
         pp_print_string !ppf " ";
         print_separated_list " " print_annoted_expression es;
         pp_print_string !ppf ")" *)
     | ELambda (_, pat, exp) ->
         pp_print_string !ppf "\\";
         print_pattern pat;   
         pp_print_string !ppf ".";
         pp_print_cut !ppf (); 
         print_expression exp;
     | EAppAnnoted (_, t1, t2, annot) ->
         pp_print_string !ppf "(";
         print_expression t1;
         pp_print_string !ppf " ";
         print_annoted_expression (t2,annot); 
         pp_print_string !ppf ")";       
     | EMatchAnnoted (_, e, annot, cs) ->
         pp_open_hvbox !ppf 0; 
         pp_print_string !ppf "match ";
         print_annoted_expression (e,annot);  
         pp_print_string !ppf " with ";
         pp_print_cut !ppf ();
         print_separated_list "| " print_clause cs;
         pp_print_string !ppf " end ";
         pp_close_box !ppf ();
     | EBinding (_, bd, exp) -> 
         pp_open_box !ppf 0;
         print_binding bd;
         pp_print_string !ppf " in ";
         print_expression exp;
         pp_close_box !ppf ();
     | ETypeConstraint (_, exp, etyp) -> 
         pp_print_string !ppf "(";
         print_expression exp;
         pp_print_string !ppf " : ";
         print_existential_type etyp;
         pp_print_string !ppf ")"
      | EImpredicativity (_, expr, vs, opt_typ1, opt_typ2) ->
         pp_print_string !ppf "(";
         print_expression expr;
         pp_print_string !ppf " : "; 
         if vs <> [] then (
           pp_print_string !ppf "?";
           print_separated_list " " print_string vs;
           pp_print_string !ppf "."
         );
         begin match opt_typ1 with
          | Some typ1->
            print_type typ1;
          | None -> ()
         end;
         pp_print_string !ppf " <F ";
         begin match opt_typ2 with
          | Some typ2 ->
            print_type typ2;
          | None -> ()
         end;
         pp_print_string !ppf ")"
     | EForall (_, vs, e) ->
         fprintf !ppf "forall ";
         print_separated_list " " (pp_print_string !ppf) vs;
         pp_print_string !ppf ".";
         print_expression e
     | EExists (_, vs, e) ->
         fprintf !ppf "exists ";
         print_separated_list " " (pp_print_string !ppf) vs;
         pp_print_string !ppf ".";
         print_expression e
     | ERecordEmpty _ ->
         pp_print_string !ppf "{}"
     | ERecordAccess (_, exp, name) -> 
         print_expression exp;
         pp_print_string !ppf ".";
         pp_print_string !ppf name;
     | ERecordExtend (_, bds, ERecordEmpty _) -> 
         pp_print_string !ppf "{ ";
         print_record_binding bds;
         pp_print_string !ppf "} ";
     | ERecordExtend _ -> assert false
     | ERecordUpdate (_, exp, label, v) -> 
         print_expression exp;
         pp_print_string !ppf ".";
         pp_print_string !ppf label;
         pp_print_string !ppf " <- ";
         print_expression v);

and print_record_binding bds = 
  print_separated_list ";" 
    (fun (name, exp) ->
       pp_print_string !ppf (name^" = ");
       print_expression exp) bds

and print_binding b =
  (match b with
     | BindValue (pos, vs) ->
         pp_print_string !ppf "let ";
         print_separated_list " and " print_value_definition vs;
     | BindRecValue (pos, vs) ->
         pp_print_string !ppf "let rec ";
         print_separated_list " and " print_value_definition vs;
     | TypeDec (_, tds) ->
         pp_print_string !ppf "type ";
         print_separated_list " and " print_type_declaration tds)

and print_value_definition (VDefAnnoted (_, tnames, pat, expression, etyp_opt)) = 
  let rec chop_args pat expression =  (* TODO_DISPLAY of annotation *)
    match (pat, expression) with
      | ((PTypeConstraint (_, PVar (m, name), _)) as p), 
         ELambda(_, pat, expression) ->
          let q, exp = chop_args pat expression in
            p :: q, exp
          
      | (PVar (m, name) as p, ELambda(_, pat, expression)) ->
          let q, exp = chop_args pat expression in
            p :: q, exp
      | (pat, expression) -> [pat], expression
  in
  let print (pats, expression) = (
    pp_open_box !ppf 0;
    if tnames <> [] then (
      pp_print_string !ppf "forall ";
      print_separated_list " " (fun s -> pp_print_string !ppf s) tnames;
      pp_print_string !ppf ".";
      pp_print_cut !ppf ()
    );
    print_separated_list " " print_pattern pats;
    begin
      match etyp_opt with 
      | Some etyp ->
        pp_print_string !ppf " :? ";
        print_existential_type etyp;   
      | None -> ()
    end;
    pp_print_string !ppf " = ";
    pp_close_box !ppf ();
    pp_print_cut !ppf (); 
    print_expression expression;
  )
  in
    print (chop_args pat expression)

and print_pattern p = 
  (match p with
     | PPrimitive _ -> assert false
     | PZero _ -> pp_print_string !ppf "ZERO"
     | PVar (_, name) -> pp_print_string !ppf name
     | PWildcard _ -> pp_print_string !ppf "_"
     | PAlias (_, name, pattern) -> 
         pp_print_string !ppf "("; 
         print_pattern pattern; 
         pp_print_string !ppf " as ";
         pp_print_string !ppf ")"
     | PTypeConstraint (_, pat, ety) -> 
         pp_print_string !ppf "(";
         print_pattern pat;
         pp_print_string !ppf " : ";
         print_existential_type ety;
         pp_print_string !ppf ")"
     | PData (_, [], "_Unit", ps) -> 
         pp_print_string !ppf "()"
     | PData (_, [], "_Tuple", ps) -> 
         if ps <> [] then (
           pp_print_string !ppf "(";
           print_separated_list "," print_pattern ps;
           pp_print_string !ppf ")")
     | PData (_, localvars,  k, ps) -> 
         pp_print_string !ppf "(";
         pp_print_string !ppf k;
         pp_print_string !ppf " ";
         if localvars <> [] then
           (
             pp_print_string !ppf "[";
             print_separated_list " " (pp_print_string !ppf) localvars;
             pp_print_string !ppf "] "
           );
         if ps <> [] then print_separated_list " " print_pattern ps;
         pp_print_string !ppf ")"
     | POr (_, [ x ]) | PAnd (_, [ x ]) ->
         print_pattern x
     | PAnd (_, ps) ->
         pp_print_string !ppf "(";
         print_separated_list "&" print_pattern ps;
         pp_print_string !ppf ")";
     | POr (_, ps) ->
         pp_print_string !ppf "(";
         print_separated_list "|" print_pattern ps;
         pp_print_string !ppf ")");

and print_type ?paren = function
  | TypVar (_, name) ->
      pp_print_string !ppf name
  | TypApp (_, TypVar(_, "->"), [t; t']) ->
      if paren <> None then 
        pp_print_string !ppf "(";
      print_type ~paren:true t;
      pp_print_string !ppf " -> ";
      print_type t';
      if paren <> None then 
        pp_print_string !ppf ")"
  | TypApp (_, TypVar(_,"*"), [a; b]) ->
      print_type ~paren:true a;
      pp_print_string !ppf " * ";
      print_type ~paren:true b
  | TypApp (_, t, []) ->
      print_type t
  | TypApp (_, t, ts) -> 
      pp_print_string !ppf "(";
      print_type t; 
      pp_print_string !ppf " ";
      print_separated_list " " print_type ts;
      pp_print_string !ppf ")";
  | TypGen (_, names, t) ->
      if names <> [] then begin
        pp_print_string !ppf "forall ";
        List.iter (fun name -> pp_print_string !ppf (name ^ " ")) names;
        pp_print_string !ppf ". ";
      end;
      print_type t 
  | TypRowCons (_, ls, ty) -> 
      pp_print_string !ppf "{";
      let print_label (l, ty) =
        pp_print_string !ppf l;
        pp_print_string !ppf " : ";
        print_type ty
      in
        print_separated_list "; " print_label ls;
        pp_print_string !ppf "; ";
        print_type ty;
        pp_print_string !ppf "}"
  | TypRowUniform (_, ty) -> 
      pp_print_string !ppf "\\";
      print_type ty

and print_existential_type (vs, typ) =
   if vs <> [] then (
     fprintf !ppf "? ";
     print_separated_list " " (pp_print_string !ppf) vs;
     pp_print_string !ppf "."
   );
   print_type typ
  
and print_kind = function
  | (KInVariant | KCoVariant | KContraVariant) as kvar -> 
    pp_print_string !ppf (symbol_of_kvariance kvar)

  | KArrow ((KInVariant | KCoVariant | KContraVariant) as k1, k2) -> 
      print_kind k1;
      pp_print_string !ppf " ->";
      print_kind k2
  | KArrow (k1, k2) -> 
      pp_print_string !ppf "(";
      print_kind k1;
      pp_print_string !ppf ") -> ";
      print_kind k2
  | _ -> assert false

  
and print_annoted_expression (expr,etyp_opt) =
    match etyp_opt with 
    | Some etyp ->
      pp_print_string !ppf "[";
      print_expression expr;
      pp_print_string !ppf " :? ";
      print_existential_type etyp;       
      pp_print_string !ppf "]"
    | None ->
      print_expression expr
    
and print_type_declaration (_, kind, n, typ) = 
  pp_open_box !ppf 0;
  pp_print_string !ppf n;
  pp_print_string !ppf " : ";
  print_kind kind;
  pp_print_string !ppf " = ";      
  pp_close_box !ppf ();
  pp_print_cut !ppf ();
  pp_open_box !ppf 0;
  print_type_definition typ;
  pp_print_string !ppf " ";
  pp_close_box !ppf ()

and print_type_definition = function
  | DAlgebraic ds -> 
      let first = ref true in
      let print_datatype_def (_, k, ty) = 
        if !first then (
          first := false) 
        else (pp_print_cut !ppf (); pp_print_string !ppf "| ");
        pp_print_string !ppf k;
        pp_print_string !ppf " : ";
        print_type ty
      in
        print_separated_list "" print_datatype_def ds;

  | DAbbrev ty -> 
      print_type ty

and print_clause (_, pat, expression) =
  print_pattern pat;
  pp_print_string !ppf " => ";
  print_expression expression;
  pp_print_cut !ppf ()

let current_active_mode = ref (Txt (Channel stdout))

let active_mode mode = 
  current_active_mode := mode;
  match mode with
    | Formatter r ->
        set_all_formatter_output_functions 
          ~out:r.out
          ~flush:r.flush 
          ~newline:r.newline 
          ~spaces:r.spaces      
          
    | Latex out -> 
        let _ = Format.set_margin 80 in
        let outputf s p n =
          let s = (String.sub s p n) in
          let s' = 
            List.fold_left 
              (fun acu (s, s') -> 
                 Str.global_replace (Str.regexp s) s' acu)
              s [ 
                ("{", "\\{");
                ("}", "\\}");
                ("|", "$|$");
                ("\\", "$\\partial$");
                ("let rec ", "\\textbf{let rec} ");
                ("let ", "\\textbf{let} ");
                ("match ", "\\textbf{match} ");
                ("end ", "\\textbf{end} ");
                ("with ", "\\textbf{with} ");
                ("type ", "\\textbf{type} ");
                ("in ", "\\textbf{in} ");
                ("=>", "$\\Rightarrow$");
                ("*", "$\\times$");
                ("forall", "$\\forall$"); 
                ("exists", "$\\exists$"); 
                ("->", " $\\rightarrow$ ");
                ("<", "$<$"); 
                (">", "$>$"); 
              ] in
          let s' = 
            List.fold_left 
              (fun acu (s, s') -> 
                 Str.global_replace (Str.regexp s) s' acu)
              s' [ ("_", "\\_") ]
          in
            output_string out s'
        and flush () = flush out
        and newline () = output_string out "\\par\n"
        and spaces n = if (n != 0) then
          output_string out 
            ("\\hspace*{"^string_of_int n^"\\smallskipamount}") in
          set_all_formatter_output_functions 
            ~out:outputf 
            ~flush:flush 
            ~newline:newline 
            ~spaces:spaces 

    | Txt out ->          
        let _ = Format.set_margin 80 in
          set_all_formatter_output_functions 
            ~out:(fun s b e -> output_string out (String.sub s b e))
            ~flush:(fun () -> flush out) 
            ~newline:(fun () -> output_string out "\n") 
            ~spaces:(fun n -> for i = 0 to n do output_string out " " done)

let print_program p = 
  List.iter (fun b ->
               pp_open_box !ppf 0;
               print_binding b; 
               pp_print_newline !ppf ();
               pp_print_newline !ppf ();
               pp_close_box !ppf ()) p

let as_string printer x = 
  let mode = !current_active_mode in
  let b = Buffer.create 10 in
    active_mode (Txt (Buffer b));
    printer x;
    Format.print_flush ();
    active_mode mode;
    Buffer.contents b
    
let register_tasks parse_program_task = 
  Processing.register 
    print_program_task 
    ([], ignore) 
    [ [ parse_program_task ] ] 
    (fun t -> print_program (List.hd t))
    (const false)

