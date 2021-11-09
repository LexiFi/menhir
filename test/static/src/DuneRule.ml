open Printf
open PrintSExp

let up =
  Filename.parent_dir_name

let (/) =
  Filename.concat

(* Constructing a standard [make]-like rule. *)

let target (targets : string list) =
  let keyword = if List.length targets = 1 then "target" else "targets" in
  L (atom keyword :: atoms targets)

let rule (targets : string list) (deps : string list) (action : sexp) =
  (* If the lists [targets] and [deps] are both empty, then we produce
     an "inferred rule", where dune infers the dependencies and targets.
     In that case, the "action" keyword is omitted. *)
  if targets = [] && deps = [] then
    L[A"rule"; action]
  else
    L[A"rule";
      target targets;
      L(A"deps" :: atoms deps);
      L[A"action"; action]
    ]

(* Constructing a phony rule, that is, a rule whose target is an alias. *)

let phony (alias : string) (action : sexp) =
  L[A"rule";
    L[A"alias"; A alias];
    L[A"action"; action]
  ]

(* Constructing a diff action. *)

let diff (expected : string) (actual : string) =
  L[A"diff"; A expected; A actual]

(* Constructing a system action. *)

let system command =
  L[A"system"; A (sprintf "\"%s\"" command)]

let system format =
  ksprintf system format

(* Redirecting the input channel of an action. *)

let redirect_stdin filename action =
  L[A"with-stdin-from"; A filename; action]

(* Redirecting the output channels of an action towards a file. *)

(* At the time of writing (2.7.1), dune has a bug that causes it to send
   an ill-formed command to the shell if both stdout and stderr are
   redirected to the same file. *)

let redirect_stdout filename action =
  L[A"with-stdout-to"; A filename; action]

let redirect_stderr filename action =
  L[A"with-stderr-to"; A filename; action]

let redirect_both filename action =
  L[A"with-outputs-to"; A filename; action]

(* Changing the working directory of an action. *)

let chdir directory action =
  L[A"chdir"; A directory; action]

(* Expressing the fact that an action is expected to fail. *)

let expecting_failure action =
  L[A"with-accepted-exit-codes"; L[A"not"; A"0"]; action]

let not_expecting_failure action =
  L[A"with-accepted-exit-codes"; A"0"; action]

let possibly_expecting_failure positive action =
  match positive with
  | `Positive ->
      not_expecting_failure action
  | `Negative ->
      expecting_failure action
      (* If this is a positive test, then the action *is not* expected to
         fail. Otherwise, the action *is* expected to fail. *)

(* Constructing a target that is an alias for a conjunction of targets. *)

let alias target deps =
  L[A"alias";
    L[A"name"; A target];
    Lnewline(A"deps" :: List.map (fun dep -> L[A"alias"; A dep]) deps)]
