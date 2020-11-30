(* An S-expression printer. *)

type sexp =
  | A of string
  | L of sexp list
  | Lnewline of sexp list

let atom sexp =
  A sexp

let atoms =
  List.map atom

let rec print ppf = function
  | A s ->
      Format.pp_print_string ppf s
  | L es ->
      Format.fprintf ppf "@[<2>(%a)@]" print_list es
  | Lnewline es ->
      Format.fprintf ppf "@[<v 2>(%a)@]" print_list es

and print_list ppf es =
  Format.pp_print_list ~pp_sep:Format.pp_print_space print ppf es

let show_list es =
  let ppf = Format.str_formatter in
  List.iter (fun e ->
    Format.fprintf ppf " ";
    print ppf e
  ) es;
  Format.flush_str_formatter()

let print sexp =
  Format.printf "@[<v>%a@,@]" print sexp;
  Format.print_newline()
