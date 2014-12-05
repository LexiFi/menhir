open EngineTypes

(* The LR parsing engine. *)

(* This module is used:

   - at compile time, if so requested by the user, via the --interpret options;
   - at run time, in the table-based back-end. *)

module Make (T : TABLE) = struct

  (* This propagates type and exception definitions. *)

  include T

  (* --------------------------------------------------------------------------- *)

  (* The type [void] is empty. Many of the functions below have return type
     [void]. This guarantees that they never return a value. Instead, they
     must stop by raising an exception: either [Accept] or [Error]. *)

  type void

  (* --------------------------------------------------------------------------- *)

  (* OK, OK. I said I would stop using [Obj.magic], yet here we go again.  I
     need to extend the type [T.token] with an extra element, which represents
     the [error] pseudo-token. I don't want to pay an extra box in memory or
     an extra field in the [env] record. (I have measured the cost of moving
     from 5 to 6 fields in this record to be 30%. This is more than I
     expected!) I don't want to add a branch to the type [T.token] because
     that would bother the user (that would be an incompatible change) and
     that would make some exhaustive case analyses appear non-exhaustive. So,
     here we go. We allocate a dummy box in memory and use its address as a
     unique value which cannot possibly be confused with a legit inhabitant of
     the type [token]. (Right?) *)

  let error_token : token =
    Obj.magic (ref 0xDEADBEEF)

  (* --------------------------------------------------------------------------- *)

  (* In the code-based back-end, the [run] function is sometimes responsible
     for pushing a new cell on the stack. This is motivated by code sharing
     concerns. In this interpreter, there is no such concern; [run]'s caller
     is always responsible for updating the stack. *)

  (* In the code-based back-end, there is a [run] function for each state
     [s]. This function can behave in two slightly different ways, depending
     on when it is invoked, or (equivalently) depending on [s].

     If [run] is invoked after shifting a terminal symbol (or, equivalently,
     if [s] has a terminal incoming symbol), then [run] discards a token,
     unless [s] has a default reduction on [#]. (Indeed, in that case,
     requesting the next token might drive the lexer off the end of the input
     stream.)

     If, on the other hand, [run] is invoked after performing a goto transition,
     or invoked directly by an entry point, then there is nothing to discard.

     These two cases are reflected in [CodeBackend.gettoken].

     Here, the code is structured in a slightly different way. It is up to the
     caller of [run] to indicate whether to discard a token, via the parameter
     [please_discard]. This flag is set when [s] is being entered by shifting
     a terminal symbol and [s] does not have a default reduction on [#]. *)

  let rec run env please_discard : void =

    (* Log the fact that we just entered this state. *)
    
    Log.state env.current;

    (* If [please_discard] is set, discard a token and fetch the next one.
       This is done by jumping to [discard], which itself will jump to
       [check_for_default_reduction]. If [please_discard] is not set, we skip
       [discard] and jump directly to [check_for_default_reduction]. *)

    if please_discard then
      discard env
    else
      check_for_default_reduction env

  (* [discard] takes a token off the input stream, queries the lexer
     for a new one, and stores it into [env.triple], overwriting the
     previous token. *)

  and discard env =
    let lexbuf = env.lexbuf in
    let token = env.lexer lexbuf in
    let startp = lexbuf.Lexing.lex_start_p
    and endp = lexbuf.Lexing.lex_curr_p in
    let triple = (startp, token, endp) in
    Log.lookahead_token startp (T.token2terminal token) endp;
    let env = { env with triple } in
    check_for_default_reduction env

  and check_for_default_reduction env =

    (* Examine what situation we are in. This case analysis is analogous to
       that performed in [CodeBackend.gettoken], in the sub-case where we do
       not have a terminal incoming symbol. *)

    T.default_reduction
      env.current
      reduce                (* there is a default reduction; perform it *)
      check_for_error_token (* there is none; continue below *)
      env

  and check_for_error_token env : void =

    (* There is no default reduction. Consult the current lookahead token
       so as to determine which action should be taken. *)

    (* Peeking at the first input token, without taking it off the input
       stream, is done by reading [env.triple]. We are careful to first
       check whether this is the [error] token. *)

    (* Note that, if [please_discard] was true, then we have just called
       [discard], so the lookahead token cannot be [error]. *)

    let (_, token, _) = env.triple in
    if token == error_token then begin
      Log.resuming_error_handling();
      error env
    end
    else

      (* We consult the two-dimensional action table, indexed by the
         current state and the current lookahead token, in order to
         determine which action should be taken. *)

      T.action
        env.current                    (* determines a row *)
        (T.token2terminal token)       (* determines a column *)
        (T.token2value token)
        shift                          (* shift continuation *)
        reduce                         (* reduce continuation *)
        initiate                       (* failure continuation *)
        env

  (* --------------------------------------------------------------------------- *)

  (* This function takes care of shift transitions along a terminal symbol.
     (Goto transitions are taken care of within [reduce] below.) The symbol
     can be either an actual token or the [error] pseudo-token. *)

  (* Here, the lookahead token CAN be [error]. *)

  and shift env
      (please_discard : bool)
      (terminal : terminal)
      (value : semantic_value)
      (s' : state)
      : void =

    (* Log the transition. *)

    Log.shift terminal s';

    (* Push a new cell onto the stack, containing the identity of the
       state that we are leaving. *)

    let (startp, _, endp) = env.triple in
    let stack = {
      state = env.current;
      semv = value;
      startp;
      endp;
      next = env.stack;
    } in

    (* Switch to state [s']. *)

    let current = s' in
    let env = { env with stack; current } in
    run env please_discard

  (* --------------------------------------------------------------------------- *)

  (* This function takes care of reductions. *)

  (* Here, the lookahead token CAN be [error]. *)

  and reduce env (prod : production) : void =

    (* Log a reduction event. *)

    Log.reduce_or_accept prod;

    (* Invoke the semantic action. The semantic action is responsible for
       truncating the stack and pushing a new cell onto the stack, which
       contains a new semantic value. It can raise [Accept] or [Error]. *)

    (* If the semantic action terminates normally, it returns a new stack,
       which becomes the current stack. *)

    (* If the semantic action raises [Error], we catch it immediately and
       initiate error handling. *)

    (* The apparently weird idiom used here is an encoding for a
       [let/unless] construct, which does not exist in ocaml. *)

    let success =
      try
	Some (T.semantic_action prod env)
      with Error ->
	None
    in
    match success with
    | Some stack ->

        (* By our convention, the semantic action has produced an updated
           stack. The state now found in the top stack cell is the return
           state. *)

        (* Perform a goto transition. The target state is determined
           by consulting the goto table at the return state and at
           production [prod]. *)

        let current = T.goto stack.state prod in
        let env = { env with stack; current } in
        run env false

    | None ->
        initiate env


  (* --------------------------------------------------------------------------- *)

  (* The following functions deal with errors. *)

  (* [initiate] initiates or resumes error handling. *)

  (* Here, the lookahead token CAN be [error]. *)

  and initiate env : void =
    Log.initiating_error_handling();
    let (startp, _, endp) = env.triple in
    let triple = (startp, error_token, endp) in
    let env = { env with triple } in
    error env

  (* [error] handles errors. *)

  and error env : void =
    assert (let (_, token, _) = env.triple in token == error_token);

    (* Consult the column associated with the [error] pseudo-token in the
       action table. *)

    T.action
      env.current                    (* determines a row *)
      T.error_terminal               (* determines a column *)
      T.error_value
      error_shift                    (* shift continuation *)
      error_reduce                   (* reduce continuation *)
      error_fail                     (* failure continuation *)
      env

  and error_shift env please_discard terminal value s' =

    (* Here, [terminal] is [T.error_terminal], and [value] is [T.error_value]. *)

    assert (terminal = T.error_terminal && value = T.error_value);

    (* This state is capable of shifting the [error] token. *)

    Log.handling_error env.current;
    shift env please_discard terminal value s'

  and error_reduce env prod =

    (* This state is capable of performing a reduction on [error]. *)

    Log.handling_error env.current;
    reduce env prod

  and error_fail env =

    (* This state is unable to handle errors. Attempt to pop a stack
       cell. *)

    let cell = env.stack in
    let next = cell.next in
    if next == cell then

      (* The stack is empty. Die. *)

      raise Error

    else begin

      (* The stack is nonempty. Pop a cell, updating the current state
	 with that found in the popped cell, and try again. *)

      let env = { env with
        stack = next;
        current = cell.state
      } in
      error env

    end

  (* --------------------------------------------------------------------------- *)

  let entry
      (s : state)
      (lexer : Lexing.lexbuf -> token)
      (lexbuf : Lexing.lexbuf)
      : semantic_value =

    (* Build an empty stack. This is a dummy cell, which is its own
       successor. Its fields other than [next] contain dummy values. *)

    let rec empty = {
      state = s;                          (* dummy *)
      semv = T.error_value;               (* dummy *)
      startp = Lexing.dummy_pos;          (* dummy *)
      endp = Lexing.dummy_pos;            (* dummy *)
      next = empty;
    } in

    (* Perform an initial call to the lexer. *)

    let token = lexer lexbuf in
    let startp = lexbuf.Lexing.lex_start_p
    and endp = lexbuf.Lexing.lex_curr_p in

    (* Log our first lookahead token. *)

    Log.lookahead_token startp (T.token2terminal token) endp;

    (* Build an initial environment. *)

    let triple = (startp, token, endp) in
    let env = {
      lexer;
      lexbuf;
      triple;
      stack = empty;
      current = s;
    } in

    (* Run. Catch [Accept], which represents normal termination. Let [Error]
       escape. *)

    try

      (* If ocaml offered a [match/with] construct with zero branches, this is
	 what we would use here, since the type [void] has zero cases. *)

      let (_ : void) = run env false in
      assert false (* cannot fail *)

    with
    | Accept v ->
	v

end

