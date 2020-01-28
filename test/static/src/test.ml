open Sys
open Array
open List
open Filename
open Printf
open Auxiliary

(* This script produces the file [dune.auto], which describes the tests we
   would like dune to execute. *)

(* Note: the contents of the .conflicts and .automaton files are not tested. *)

(* -------------------------------------------------------------------------- *)

(* Settings. *)

let extra : string list ref =
  ref []

let verbosity =
  ref 0

let usage =
  sprintf "Usage: %s\n" argv.(0)

let spec = Arg.align [
  "--extra-flags",     Arg.String (fun flag -> extra := flag :: !extra),
                       "<string> specify extra flags for Menhir";
  "--verbosity",       Arg.Int ((:=) verbosity),
                       " set the verbosity level (0-2)";
]

let () =
  Arg.parse spec (fun _ -> ()) usage

let extra : string list =
  List.rev !extra

let verbosity =
  !verbosity

(* -------------------------------------------------------------------------- *)

(* Paths. *)

let good =
  Filename.concat Filename.parent_dir_name "good"

let good_slash filename =
  Filename.concat good filename

let bad =
  Filename.concat Filename.parent_dir_name "bad"

let bad_slash filename =
  Filename.concat bad filename

(* -------------------------------------------------------------------------- *)

(* Test files and groups of test files. *)

let id basenames =
  (* A name for a nonempty group of test files. *)
  hd basenames

let thisfile basenames =
  if length basenames > 1 then "these input files" else "this input file"

let mly basename =
  basename ^ ".mly"

let mlys =
  map mly

(* -------------------------------------------------------------------------- *)

(* Test inputs and outputs. *)

(* A test input is a list of basenames, without the .mly extension.
   These files must be passed together to menhir. *)

type input =
  | NegativeTest of filename list
  | PositiveTest of filename list

type inputs = input list

(* -------------------------------------------------------------------------- *)

(* An S-expression printer. *)

type sexp =
  | A of string
  | L of sexp list
  | Lnewline of sexp list

let atom sexp =
  A sexp

let atoms =
  List.map atom

let rec print_sexp ppf = function
  | A s ->
      Format.pp_print_string ppf s
  | L l ->
      Format.fprintf ppf "@[<2>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_sexp) l
  | Lnewline l ->
      Format.fprintf ppf "@[<v 2>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_sexp) l

let print_sexp sexp =
  Format.printf "@[<v>%a@,@]" print_sexp sexp;
  Format.print_newline()

(* -------------------------------------------------------------------------- *)

(* Constructing a standard [make]-like rule. *)

let rule (target : string) (deps : string list) (action : sexp) =
  L[A"rule";
    L[A"target"; A target];
    L(A"deps" :: atoms deps);
    L[A"action"; action]
  ]

let print_rule target deps action =
  print_sexp (rule target deps action)

(* Constructing a phony rule, that is, a rule whose target is an alias. *)

let phony (alias : string) (action : sexp) =
  L[A"rule";
    L[A"alias"; A alias];
    L[A"action"; action]
  ]

let print_phony alias action =
  print_sexp (phony alias action)

(* Constructing a diff action. *)

let diff (expected : string) (actual : string) =
  L[A"diff"; A expected; A actual]

(* -------------------------------------------------------------------------- *)

(* Calling conventions for Menhir. *)

(* A --base option is needed for groups of several files. *)

let base basenames =
  if length basenames > 1 then
    let id = id basenames in
    [A"--base"; A id]
  else
    []

(* The Menhir command. *)

(* This command is meant to be used inside a rule. *)

let menhir base flags =
  L(A"run" :: A"menhir" :: base @ flags @ [A"%{deps}"])

(* -------------------------------------------------------------------------- *)

(* Running a negative test. *)

let process_negative_test basenames : unit =

  (* Compute the name of this test. *)
  let id = id basenames in

  (* A --base option is needed for groups of several files. *)
  let base = base basenames in

  (* The output is stored in this file. *)
  let result = id ^ ".result" in

  (* Flags. *)
  let flags = id ^ ".flags" in
  let flags =
    let extra = atoms extra in
    if file_exists (bad_slash flags) then
      A(sprintf "%%{read-lines:%s}" (bad_slash flags)) :: extra
    else
      extra
  in

  (* Run Menhir in the directory bad/. *)
  print_rule
    result
    (List.map bad_slash (mlys basenames))
    (L[A"with-outputs-to"; A "%{target}";
       L[A"chdir"; A bad;
         L[A"with-accepted-exit-codes"; L[A"not"; A"0"];
           menhir base flags]]]);

  (* Check that the output coincides with what was expected. *)
  let expected = id ^ ".expected" in
  print_phony
    id
    (diff (bad_slash expected) result)

(* -------------------------------------------------------------------------- *)

(* Running a positive test. *)

(*
  Conventions:
  The file %.flags   (if it exists) stores flags for Menhir.
  The file %.opp.out stores the output of menhir --only-preprocess.
  The file %.opp.exp stores its expected output.
  The file %.out     stores the output of menhir.
  The file %.exp     stores its expected output.
 *)

let process_positive_test basenames : unit =

  (* Compute the name of this test. *)
  let id = id basenames in

  (* A --base option is needed for groups of several files. *)
  let base = base basenames in

  (* Flags. *)
  let flags = id ^ ".flags" in
  let flags =
    let extra = atoms extra in
    if file_exists (good_slash flags) then
      A(sprintf "%%{read-lines:%s}" (good_slash flags)) :: extra
    else
      extra
  in

  (* Run menhir --only-preprocess. *)
  let oppout = id ^ ".opp.out" in
  print_rule
    oppout
    (List.map good_slash (mlys basenames))
    (L[A"with-outputs-to"; A"%{target}";
       L[A"chdir"; A good;
         menhir base (A"--only-preprocess" :: flags)]]);

  (* Check that the output coincides with what was expected. *)
  let oppexp = id ^ ".opp.exp" in
  print_phony
    id
    (diff (good_slash oppexp) oppout);

  (* Run menhir. *)
  let out = id ^ ".out" in
  print_rule
    out
    (List.map good_slash (mlys basenames))
    (L[A"with-outputs-to"; A"%{target}";
       L[A"chdir"; A good;
         menhir base (
           A"--explain" :: A"-lg" :: A"2" :: A"-la" :: A"2" :: A"-lc" :: A"2" ::
           flags)]]);

  (* Check that the output coincides with what was expected. *)
  let exp = id ^ ".exp" in
  print_phony
    id
    (diff (good_slash exp) out)

(* -------------------------------------------------------------------------- *)

(* Running a test. *)

let process input =
  match input with
  | NegativeTest basenames ->
      process_negative_test basenames
  | PositiveTest basenames ->
      process_positive_test basenames

let id input =
  match input with
  | NegativeTest basenames
  | PositiveTest basenames ->
      id basenames

(* -------------------------------------------------------------------------- *)

(* [run] runs a bunch of tests in parallel. *)

let run (inputs : inputs) =
  List.iter process inputs;
  let ids = List.map id inputs in
  let ids = List.sort_uniq compare ids in
  print_sexp
    (L[A"alias";
       L[A"name"; A"test"];
       Lnewline(A"deps" :: List.map (fun id -> L[A"alias"; A id]) ids)])

(* -------------------------------------------------------------------------- *)

(* Main. *)

(* Menhir can accept several .mly files at once. By convention, if several
   files have the same name up to a numeric suffix, then they belong in a
   single group and should be fed together to Menhir. *)

let inputs directory : filename list list =
     readdir directory
  |> to_list
  |> filter (has_suffix ".mly")
  |> map chop_extension
  |> sort compare
  |> groups equal_up_to_numeric_suffix

let positive : inputs =
     inputs good
  |> map (fun basenames -> PositiveTest basenames)

let negative : inputs =
     inputs bad
  |> map (fun basenames -> NegativeTest basenames)

let inputs =
  positive @ negative

let () =
  print_endline
    ";; This file has been auto-generated. Please do not edit it.\n\
     ;; Instead, edit [test.ml] and run [make depend].\n"

let () =
  run inputs
