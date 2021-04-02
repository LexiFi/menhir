Require Import ExtrOcamlIntConv.
Require MiniCalc.Parser.
Require Extraction.

(* Datatypes *)
Extract Inlined Constant Datatypes.fst => "fst".
Extract Inlined Constant Datatypes.snd => "snd".
Extract Inductive prod => "( * )" [ "" ].

Extract Inlined Constant Parser.Ast.string => "String.t".
Extract Constant Parser.Ast.loc => "Lexing.position * Lexing.position".

Set Extraction AccessOpaque.
Set Warnings "-extraction-opaque-accessed".

Cd "extraction".

Separate Extraction nat_of_int int_of_n.
Recursive Extraction Library Parser.
