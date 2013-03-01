open Printf
open Print
open Typed

(* [print_value] is a value printer. It is parameterized over a variable
   printer. *)

let rec print_value print_variable buffer v =
  match v with
  | VVar x ->
      print_variable buffer x
  | VBool b ->
      bprintf buffer "%b" b
  | VTagTuple (tag, tuple, _) ->
      let components = Layout.fold (fun c cs -> c :: cs) tuple [] in
      bprintf buffer "%s%a"
	(SymbolTable.tagname tag)
	(print_components print_variable) components

and print_components print_variable buffer = function
  | [] ->
      ()
  | components ->
      bprintf buffer " (%a)" (seplist comma (print_value print_variable)) components

