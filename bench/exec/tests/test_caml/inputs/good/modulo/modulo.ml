(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/modulo.ml,v 1.30 2002/05/25 13:16:53 fpottier Exp $ *)

let load filename =

  let lexbuf = Lexing.from_channel (open_in filename) in

  (* Printing a term in raw (internal) and pretty (external) syntax. *)

  let print x =
    let tree = Unification.convert x in
    Unification.print tree, Conversion.print (Conversion.invert (Boil.convert tree))
  in

  (* Printing the remaining constraints. *)

  let equation (a1, a2) =
    let raw1, pretty1 = print a1 in
    let raw2, pretty2 = print a2 in
    Printf.sprintf "%s = %s\n" pretty1 pretty2 (* raw1 raw2 *)
  in

  let equations () =
    let s = Unification.equations (fun s eq ->
      s ^ (equation eq)
    ) "" in
    if s <> "" then Printf.sprintf "Remaining equations:\n\n%s\n" s else s
  in

  (* Printing the types of those sub-expressions marked by the user. *)

  let display () =
    List.fold_left (fun s x ->
      Unification.strong x;
      let raw, pretty = print x in
      Printf.sprintf "%sRaw:    %s\nPretty: %s\n\n" s raw pretty
    ) "" !Boil.display
  in

  let msg =
    try

      (* TEMPORARY kind checking *)

      Boil.check (Conversion.convert (Parser.program Lexer.token lexbuf));
      Unification.solve();

      display()

    with Unification.Inconsistency eq ->
      Printf.sprintf "*** Error: %s\n%s\n" (equation eq) (Unification.info eq)

  in
  print_endline (msg ^ (equations()))

let p =
  if not !Sys.interactive then begin
    Arg.parse [] load "Usage: modulo <filename>\n  where <filename> contains a program or a unification problem.\n";
    flush stdout;
    exit(0)
  end
  else begin (* TEMPORARY used for testing within O'Caml toplevel *)
    let lexbuf = Lexing.from_channel (open_in "essai") in
    Conversion.convert (Parser.program Lexer.token lexbuf)
  end

(*

#load "standard.cmo";;
#load "id.cmo";;
#load "unionFind.cmo";;
#load "fQueue.cmo";;
#load "tree.cmo";;
#load "unification.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
#load "internalSyntax.cmo";;
#load "conversion.cmo";;
#load "boil.cmo";;
open InternalSyntax;;
#print_depth 32000;;
#print_length 32000;;
#use "modulo.ml";;

*)

