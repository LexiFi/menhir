(* This file defines an abstract machine based on the Modern SECD. *)

open Printf
open AbstractSyntax

(* The instruction set for this machine.  See comments in file machine.mli. *)

type instruction =
  | IAccess of int
  | IClosure of code
  | IRecClosure of code
  | ITailapply
  | IApply
  | IReturn
  | ILet
  | IEndlet of int
  | IDup
  | IDrop
  | IConstr of data_constructor * int
  | IField of int
  | IIfconstr of data_constructor * code * code
  | IHalt

and code = instruction list

(* A pretty-printer for machine code. *)

let print_indent depth =
  for i = 1 to depth do print_string "    " done

let rec print_instr indent i =
  match i with
  | IAccess n ->
      print_indent indent; printf "access(%d)\n" n
  | IClosure c ->
      print_indent indent; printf "closure {\n";
      print_code (indent + 1) c;
      print_indent indent; printf "}\n";
  | IRecClosure c ->
      print_indent indent; printf "recclosure {\n";
      print_code (indent + 1) c;
      print_indent indent; printf "}\n";
  | ITailapply ->
      print_indent indent; printf "tailapply\n"
  | IApply ->
      print_indent indent; printf "apply\n"
  | IReturn ->
      print_indent indent; printf "return\n"
  | ILet ->
      print_indent indent; printf "let\n"
  | IEndlet n ->
      print_indent indent; printf "endlet(%d)\n" n
  | IDup ->
      print_indent indent; printf "dup\n"
  | IDrop ->
      print_indent indent; printf "drop\n"
  | IConstr(cstr, arity) ->
      print_indent indent; printf "constr(%s, %d)\n" cstr arity
  | IField n ->
      print_indent indent; printf "field(%d)\n" n
  | IIfconstr(cstr, ifso, ifnot) ->
      print_indent indent; printf "ifconstr(%s) {\n" cstr;
      print_code (indent + 1) ifso;
      print_indent indent; printf "} else {\n";
      print_code (indent + 1) ifnot;
      print_indent indent; printf "}\n"
  | IHalt ->
      print_indent indent; printf "halt\n"

and print_code indent c =
  List.iter (print_instr indent) c

let print_program prog =
  print_code 1 prog

(* Representation of machine values and environments *)

type value =
  | Con of data_constructor * value list
  | Closure of code * environment

and environment = value list

(* Print a value on standard output *)

let rec print_value v =
  match v with
  | Con(cstr, []) ->
      printf "%s" cstr
  | Con(cstr, v1 :: vl) ->
      printf "%s(" cstr;
      print_value v1;
      List.iter (fun v -> printf ", "; print_value v) vl;
      printf ")"
  | Closure(_, _) ->
      printf "<fun>"

(* Exception raised when the machine gets stuck. *)
exception MachineError of string

(* Representation of the stack. *)

type stack_slot =
  | Val of value
  | Return_frame of code * environment 

type stack = stack_slot list

(* A tracing facility.  If the [trace] reference is set to [true],
   the machine will print its state before each transition.
   Very useful to debug a compiler. *)

let trace = ref false

let print_state (c, e, s) =
  printf "***********************************\n";
  printf "Instruction:\n";
  begin match c with
  | [] -> printf "    <none>\n"
  | i :: _ -> print_instr 1 i
  end;
  printf "Environment:\n    ";
  List.iter
    (function v -> print_value v; printf " ")
    e;
  printf "\n";
  printf "Stack:\n    ";
  List.iter
    (function Val v -> print_value v; printf " "
            | Return_frame(_, _) -> printf "<ret> ")
    s;
  printf "\n"
  
(* Extract the [arity] top entries of the stack. *)
let split_stack arity stack =
  let rec split arity stack args =
    match arity, stack with
    | 0, _ -> (args, stack)
    | _, Val v1 :: s' -> split (arity - 1) s' (v1 :: args)
    | _, _ -> raise (MachineError "wrong stack for IConstr")
  in split arity stack []

(* Remove the first [n] entry from the environment *)
let rec drop_env n env =
  match n, env with
  | 0, _ -> env
  | _, v :: env' -> drop_env (n-1) env'
  | _, _ -> raise (MachineError "wrong environment for IEndlet")

(* The transition function for the machine.  Takes the current state
   as argument, returns the next state as result. *)

let transition = function
  | (IAccess n :: c, e, s) ->
      let vn =
        try List.nth e n
        with Failure _ -> raise (MachineError "unbound variable") in
      (c, e, Val vn :: s)
  | (IClosure c' :: c, e, s) ->
      let clos = Closure(c', e) in
      (c, e, Val clos :: s)
  | (IRecClosure c' :: c, e, s) ->
      let rec clos = Closure(c', clos :: e) in
      (c, e, Val clos :: s)
  | (ITailapply :: c, e, Val v :: Val (Closure(c', e')) :: s) ->
      (c', v :: e', s)
  | (IApply :: c, e, Val v :: Val (Closure(c', e')) :: s) ->
      (c', v :: e', Return_frame(c, e) :: s)
  | (IReturn :: c, e, Val v :: Return_frame(c', e') :: s) ->
      (c', e', Val v :: s)
  | (ILet :: c, e, Val v :: s) ->
      (c, v :: e, s)
  | (IEndlet n :: c, e, s) ->
      (c, drop_env n e, s)
  | (IDup :: c, e, Val v :: s) ->
      (c, e, Val v :: Val v :: s)
  | (IDrop :: c, e, Val v :: s) ->
      (c, e, s)
  | (IConstr(cstr, arity) :: c, e, s) ->
      let (args, s') = split_stack arity s in
      (c, e, Val(Con(cstr, args)) :: s')
  | (IField n :: c, e, Val(Con(cstr, args)) :: s) ->
      let vn =
        try List.nth args n
        with Failure _ -> raise (MachineError "wrong IField") in
      (c, e, Val vn :: s)
  | (IIfconstr(cstr, ifso, ifnot) :: c, e, Val(Con(cstr', _)) :: s) ->
      if cstr = cstr'
      then (ifso @ c, e, s)
      else (ifnot @ c, e, s)
  | (IHalt :: c, e, s) ->
      raise (MachineError "halted")
  | _ ->
      raise (MachineError "no transition is possible")

(* Repeat transitions until a final state is reached. *)
let rec exec state =
  if !trace then print_state state;
  match state with
  | ([], e, Val v :: []) -> v
  | _ -> exec (transition state)

(* Execute a code and print its result value. *)
let execute_program code =
  try
    print_value (exec (code, [], [])); print_newline()
  with MachineError msg ->
    printf "Machine error: %s\n" msg

