(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/token.ml,v 1.9 2000/02/11 16:16:34 fpottier Exp $ *)

(* These modules define how types are printed, by giving concrete meaning to abstract tokens. Two output formats are
   given: regular text and \TeX. *)

module Text = struct
  
  type token = MlAlgebra.Print.token

  open MlAlgebra.Print
  open Format

  let token = function
    | TokenArrow ->
	printf "->"
    | TokenBottom ->
	printf "0"
    | TokenLBrace ->
	printf "{ "
    | TokenRAbsent ->
	printf "RAbs"
    | TokenRBottom ->
	printf "R0"
    | TokenRBrace ->
	printf " }"
    | TokenREither ->
	printf "REither"
    | TokenRPresent ->
	printf "RPre"
    | TokenRTop ->
	printf "R1"
    | TokenRecord ->
	printf "{}"
    | TokenSpacedArrow ->
	printf "@ ->@ "
    | TokenSpacedREither ->
	printf "REither@ "
    | TokenSpacedRPresent ->
	printf "RPre@ "
    | TokenSpacedStar ->
	printf "@ *@ "
    | TokenStar ->
	printf "*"
    | TokenTop ->
	printf "1"
    | TokenInt ->
	printf "int"
    | TokenUnit ->
	printf "unit"
    | TokenBool ->
	printf "bool"
    | TokenFloat ->
	printf "float"
    | TokenChar ->
	printf "char"
    | TokenString ->
	printf "string"
    | TokenRef ->
	printf "ref"
    | TokenSpacedRef ->
	printf " ref"
    | TokenVect ->
	printf "vect"
    | TokenSpacedVect ->
	printf " vect"
    | TokenComma ->
	printf ",@ "
    | TokenLParen ->
	printf "("
    | TokenRParen ->
	printf ")"
    | TokenLBracket ->
	printf "[ "
    | TokenRBracket ->
	printf " ]"
    | TokenVAbsent ->
	printf "VAbs"
    | TokenVPresent ->
	printf "VPre"
    | TokenSpacedVPresent ->
	printf "VPre@ "
    | TokenVTop ->
	printf "V1"
    | TokenVariant ->
	printf "[]"

end

module TeX = struct
  
  type token = MlAlgebra.Print.token

  open MlAlgebra.Print

  let literal string =
    Printf.sprintf "{\\texttt{%s}}" string

  let token = function
    | TokenArrow
    | TokenSpacedArrow ->
	print_string "\\rightarrow"
    | TokenBottom ->
	print_string "\\bot"
    | TokenLBrace ->
	print_string "\\{ "
    | TokenRAbsent ->
	print_string (literal "RAbs")
    | TokenRBottom ->
	print_string "\\bot_R"
    | TokenRBrace ->
	print_string " \\}"
    | TokenREither ->
	print_string (literal "REither")
    | TokenRPresent ->
	print_string (literal "RPre")
    | TokenRTop ->
	print_string "\\top_R"
    | TokenRecord ->
	print_string "\\{\\}"
    | TokenSpacedREither ->
	print_string ((literal "REither") ^ "\\,")
    | TokenSpacedRPresent ->
	print_string ((literal "RPre") ^ "\\,")
    | TokenStar
    | TokenSpacedStar ->
	print_string "\\times"
    | TokenTop ->
	print_string "\\top"
    | TokenInt ->
	print_string (literal "int")
    | TokenUnit ->
	print_string (literal "unit")
    | TokenBool ->
	print_string (literal "bool")
    | TokenFloat ->
	print_string (literal "float")
    | TokenChar ->
	print_string (literal "char")
    | TokenString ->
	print_string (literal "string")
    | TokenComma ->
	print_string ", "
    | TokenRef ->
	print_string (literal "ref")
    | TokenSpacedRef ->
	print_string ("\\," ^ (literal "ref"))
    | TokenVect ->
	print_string (literal "vect")
    | TokenSpacedVect ->
	print_string ("\\," ^ (literal "vect"))
    | TokenLParen ->
	print_string "("
    | TokenRParen ->
	print_string ")"
    | TokenLBracket ->
	print_string "["
    | TokenRBracket ->
	print_string "]"
    | TokenVAbsent ->
	print_string (literal "VAbs")
    | TokenVPresent ->
	print_string (literal "VPre")
    | TokenSpacedVPresent ->
	print_string ((literal "VPre") ^ "\\,")
    | TokenVTop ->
	print_string "\\top_V"
    | TokenVariant ->
	print_string "[]"

end

