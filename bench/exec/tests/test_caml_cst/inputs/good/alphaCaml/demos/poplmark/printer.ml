(*
 *  A pretty-printer. Parts taken from the fullfsub implementation
 *  by the POPLmark team.
 *)

open Format
open Strings
open Fsub.Raw

let pr = print_string
let obox0 () = open_hvbox 0
let obox () = open_hvbox 2
let cbox = close_box
let break () = print_break 0 0

let printrecord f sep fields =
  pr "{";
  open_hovbox 0;
  let _ = StringMap.fold (fun li thingi first ->
    if not first then begin
      pr ",";
      print_space(); 
    end;
    pr li;
    pr sep;
    f thingi;
    false
  ) fields true in
  pr "}";
  cbox()

let rec printty_Type = function
  | TForall (tyX, tyT1, tyT2) ->
      obox(); pr "All "; pr tyX;
      proty tyT1;
      pr ".";
      print_space ();
      printty_Type tyT2;
      cbox()
  | tyT ->
      printty_ArrowType tyT

and printty_ArrowType = function
  | TArrow (tyT1, tyT2) ->
      obox0(); 
      printty_AType tyT1;
      pr " ->";
      print_space();
      printty_ArrowType tyT2;
      cbox()
  | tyT ->
      printty_AType tyT

and proty = function
  | TTop ->
      ()
  | tyS ->
      pr "<:";
      printty_Type tyS

and printty_AType = function
  | TVar x ->
        pr x
  | TTop ->
      pr "Top"
  | TRecord fields ->
      printrecord printty_Type ":" fields
  | tyT ->
      pr "(";
      printty_Type tyT;
      pr ")"

let printty = printty_Type 

let rec printpat = function
  | PWildcard ->
      pr "_"
  | PVar (x, tyT) ->
      pr x; pr ":"; printty_Type tyT
  | PRecord fields ->
      printrecord printpat "=" fields

let rec printtm_Term = function
  | EAbs (x, tyT1, t2) ->
      obox(); pr "lambda ";
      pr x; pr ":"; printty_Type tyT1; pr ".";
      print_space();
      printtm_Term t2;
      cbox()
  | ELet(p, t1, t2) ->
       obox0();
       pr "let "; printpat p; pr " = "; 
       printtm_Term t1;
       print_space(); pr "in"; print_space();
       printtm_Term t2;
       cbox()
  | ETyAbs(x, tyS, t) ->
      obox(); pr "lambda ";
      pr x; proty tyS; pr ".";
      print_space();
      printtm_Term t;
      cbox()
  | t ->
      printtm_AppTerm t

and printtm_AppTerm = function
  | EApp (t1, t2) ->
      obox0();
      printtm_AppTerm t1;
      print_space();
      printtm_PathTerm t2;
      cbox()
  | ETyApp (t, tyS) ->
      obox0();
      printtm_AppTerm t;
      print_space();
      pr "["; printty_Type tyS; pr "]";
      cbox()
  | t ->
      printtm_PathTerm t

and printtm_PathTerm = function
  | EProj(t1, l) ->
      printtm_ATerm t1; pr "."; pr l
  | t ->
      printtm_ATerm t

and printtm_ATerm = function
  | EVar x ->
      pr x
  | ERecord fields ->
      printrecord printtm_Term "=" fields
  | t ->
      pr "(";
      printtm_Term t;
      pr ")"

let printtm = printtm_Term 

let rec prtop = function
  | TopEOF ->
      ()
  | TopEval (t, rest) ->
      printtm t;
      pr ";";
      Format.force_newline();
      prtop rest
  | TopTermBind (x, tyT, rest) ->
      pr x;
      pr ":";
      printty_Type tyT;
      pr ";";
      Format.force_newline();
      prtop rest
  | TopTypeBind (x, tyS, rest) ->
      pr x;
      proty tyS;
      pr ";";
      Format.force_newline();
      prtop rest
