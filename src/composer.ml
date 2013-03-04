(* TEMPORARY we could choose not to decrement [fuel] when only one token
   exists; forced moves would be free *)

(* TEMPORARY favor reducing over shifting? this should help produce short
   continuations; also, favor reducing long productions. When shifting,
   could we somehow favor simpler alternatives? Could we use a graph
   search algorithm (IDA* ) to find an accepting sequence? *)

(* TEMPORARY warn against %nonassoc when --compose is active. *)

open EngineTypes

(* The composer is a modified parsing engine that invents its own input. It is
   meant to be invoked after the regular parsing engine encounters an error. It
   begins execution where the regular engine left off, with the same stack. *)

(* The composer requires [--canonical]. Indeed, a non-canonical LR(1) automaton
   allows too many reductions, so a composer based on such an automaton would
   sometimes invent incorrect input and crash. *)

(* The composer does not have any error handling code. In fact, unless there
   is a design mistake, it should never encounter an error, since it invents
   its own input so as to satisfy itself! (At least, this is true if the
   automaton is canonical. If %nonassoc was used to solve conflicts, we
   might in principle find a situation where both conflicting actions have
   been removed from the automaton and no suitable token remains. This
   should be quite rare.) *)

(* We use the same stack as the regular parsing engine. However, we produce
   dummy semantic values, so the [semv] field must no longer be consulted, and
   the semantic actions must no longer be invoked. The [startp] and [endp]
   fields may also contain dummy positions. *)

module Make (T : TABLE) = struct

  include T

  (* --------------------------------------------------------------------------- *)

  (* We have access to the following facilities. *)

  let (print : terminal -> string),
      (iter : (terminal -> unit) -> unit),
      (eof : terminal)
  =
    match T.compose with
    | None ->
        (* If [--compose] was disabled, we should not be here. *)
        assert false
    | Some fs ->
        fs

  (* --------------------------------------------------------------------------- *)

  (* The composer carries its own environment record. *)

  type env = {

    (* This list holds the terminal symbols that we have produced so far. They
       are stored in reverse order, that is, the most recently produced symbols
       are found near the head of the list. *)

    mutable produced: terminal list;

    (* This is the next terminal symbol. *)

    mutable token: terminal;

    (* This integer bound tells how many more symbols we are allowed to invent. *)

    mutable fuel: int;

    (* The stack. *)

    mutable stack: (state, Obj.t) stack;

    (* The current state. *)

    mutable current: state;

  }

  (* --------------------------------------------------------------------------- *)

  (* This auxiliary function picks an element at random out of a list. *)

  let pick (xs : 'a list) : 'a option =
    let n = List.length xs in
    if n = 0 then
      None
    else
      Some (List.nth xs (Random.int n))

  (* --------------------------------------------------------------------------- *)

  (* [shifts s t] indicates whether there exists a shift action for state [s]
     and token [t]. [reduces s t] indicates whether there exists a reduce action
     for state [s] and token [t]. We assume that [s] does not have a default
     reduction (this has been previously checked). *)

  let shifts (s : state) (t : terminal) : bool =
    T.action
      s
      t
      ()
      (fun () _  _ () _ -> true)  (* a shift action exists *)
      (fun () _ -> false)         (* a reduce action exists *)
      (fun () -> false)           (* no action exists *)
      ()

  let reduces (s : state) (t : terminal) : bool =
    T.action
      s
      t
      ()
      (fun () _  _ () _ -> false) (* a shift action exists *)
      (fun () _ -> true)          (* a reduce action exists *)
      (fun () -> false)           (* no action exists *)
      ()

  (* [filter p s] returns a list of all terminal symbols [t] that satisfy
     the predicate [p]. The symbols [error] and [#] are not considered. *)

  (* This inefficient implementation iterates over all terminal symbols other
     than [error] and [#]. This offers the advantage of not requiring any extra
     tables. *)

  let filter (p : terminal -> bool) : terminal list =
    let ts = ref [] in
    iter (fun t ->
      if p t then
	ts := t :: !ts
    );
    !ts

  (* --------------------------------------------------------------------------- *)

  (* [invent state] invents a terminal symbol that will not cause an error in
     the state [state]. We allow for the possibility that no such symbol might
     exist. As noted above, this could occur if %nonassoc was used. *)

  let invent (s : state) : terminal option =

    (* If [s] has a default reduction, then we invent the symbol [#].
       In principle, a canonical automaton should have a default
       reduction only on this symbol. And it is a valid choice,
       anyway. *)

    T.default_reduction
      s
      (fun () _ -> Some eof)
      (fun () ->

	(* If there is no default reduction, then we create a list
	   of all permitted terminal symbols, and pick one of them. *)
	
	(* We give absolute priority to reductions: we never shift
	   when we can reduce. This is supposed to help us propose
	   reasonable suggestions, which reduce the size of the stack. *)
	
	pick (
	  match filter (reduces s) with
	  | [] ->
	      filter (shifts s)
	  | ts ->
	      ts
	)

      )
      ()

  (* --------------------------------------------------------------------------- *)

  (* When the composer is done, it produces a list of terminal symbols. These
     symbols are translated to a list of strings, which are the symbol's
     internal names. The client can print these names (possibly after
     transforming them to a more user-readable form). *)

  (* This function is used when the composer returns. It reverses the list
     of terminal symbols (which was naturally constructed in reverse order),
     and prints them. *)

  let process (ts : terminal list) : string list =
    List.rev_map print ts

  (* --------------------------------------------------------------------------- *)

  (* [discard] takes a terminal symbol off the input stream, invents a new
     one, and stores it into [token], overwriting the previous one. We assume
     that [fuel] is nonzero; it is decremented. If it reaches zero, we stop. *)

  (* [discard] returns a Boolean success code. The value [false] means that
     we failed to invent a new terminal symbol. *)

  let discard env : bool =
    assert (env.fuel > 0);
    env.fuel <- env.fuel - 1;
    match invent env.current with
    | Some t ->
        Printf.eprintf "Picking a new symbol: %s\n" (print t); (* TEMPORARY *)
        if t <> eof then
          env.produced <- t :: env.produced;
        env.token <- t;
        true
    | None ->
        false
    (* removed a call to [Log.lookahead_token] because we do not have
       a valid [lexbuf] argument *)

  (* --------------------------------------------------------------------------- *)

  (* The code of the composer is modeled after that of the regular parsing
     engine. We do not repeat the comments and comment only on the differences. *)
 
  let rec run env please_discard : suggestion =

    let s = env.current in
    Log.state s;

    if please_discard && env.fuel = 0 then
      OutOfFuel (process env.produced)

    else begin

      let success : bool =
	if please_discard then
	  discard env
	else
	  true
      in

      if not success then
	Blocked (process env.produced)
      else

	(* By construction, a canonical automaton has no default reductions,
	   except on [#]. *)

	T.default_reduction
	  s
	  reduce (* there is a default reduction; perform it *)
	  action (* there is none; continue below *)
	  env

    end

  (* --------------------------------------------------------------------------- *)

  and action env : suggestion =

    T.action
      env.current                    (* determines a row *)
      env.token                      (* determines a column *)
      ()
      shift                          (* shift continuation *)
      reduce                         (* reduce continuation *)
      initiate                       (* failure continuation *)
      env

  (* --------------------------------------------------------------------------- *)

  and shift env
      (please_discard : bool)
      (terminal : terminal)
      (value : unit)
      (s' : state)
      : suggestion =

    Log.shift terminal s';

    (* The new cell stack that we construct holds a dummy semantic value
       and dummy positions. *)

    env.stack <- {
      state = env.current;
      semv = Obj.repr ();
      startp = Lexing.dummy_pos;
      endp = Lexing.dummy_pos;
      next = env.stack;
    };

    (* Switch to state [s']. *)

    env.current <- s';
    run env please_discard

  (* --------------------------------------------------------------------------- *)

  and reduce env (prod : production) : suggestion =

    Log.reduce_or_accept prod;

    (* If this is an accepting production, we are done. *)

    if is_start prod then
      Accepted (process env.produced)

    else begin

      (* We cannot possibly invoke the user-supplied semantic action, because
	 it expects well-formed semantic values, which we cannot supply. *)

      (* Thus, it it up to us to pop an appropriate number of cells off the
	 stack. *)

      pop (length prod) env;

      (* Perform a goto transition and push a new stack cell. *)

      let current = env.current in
      env.current <- T.goto current prod;
      env.stack <- {
	state = current;
	semv = Obj.repr ();
	startp = Lexing.dummy_pos;
	endp = Lexing.dummy_pos;
	next = env.stack;
      };

      run env false

    end

  (* --------------------------------------------------------------------------- *)

  (* [pop n env] pops [n] cells off the stack. It updates the current state as
     follows: whenever a cell is popped, the state that it contains becomes the
     current state. In other words, if [n] is zero, then the current state is
     unchanged; otherwise, the state found in the last cell that is popped becomes
     the current state. *)

  and pop n env : unit =
    if n > 0 then begin
      let stack = env.stack in
      env.current <- stack.state;
      env.stack <- stack.next;
      pop (n - 1) env
    end

  (* --------------------------------------------------------------------------- *)

  (* [initiate] should never be called. *)

  and initiate env : suggestion =
    assert false

  (* --------------------------------------------------------------------------- *)

  let entry
      (env : (state, semantic_value, token) EngineTypes.env)
      (fuel : int)
      : suggestion =

    assert (fuel >= 0);

    (* Transform the environment of the regular parsing engine into a composer
       environment. The type cast that is applied to the stack is actually safe,
       as we are casting up from the abstract type [T.semantic_value] to the
       type [Obj.t]. (Stack is covariant.) *)

    let env = {
      produced = [];
      token = T.token2terminal env.EngineTypes.token; (* dummy; will be discarded below *)
      fuel = fuel;
      stack = (Obj.magic env.EngineTypes.stack);
      current = env.EngineTypes.current;
    } in

    (* Run. *)

    (* The value [true] means that the terminal symbol that caused the error
       will be discarded and replaced with a symbol of our choosing. *)

    run env true

end

