open EngineTypes

(* The LR parsing engine. *)

(* This module is used:

   - at compile time, if so requested by the user, via the --interpret options;
   - at run time, in the table-based back-end. *)

module Make (T : TABLE) = struct

  (* This propagates type and exception definitions. *)

  include T

  type env =
      (state, semantic_value, token) EngineTypes.env

  (* --------------------------------------------------------------------------- *)

  (* The type [result] represents an intermediate or final result of the
     parser. See [EngineTypes]. *)

  (* The type [result] is presented to the user as a private type (see
     [IncrementalEngine]). This prevents the user from manufacturing results
     (i.e., continuations) that do not make sense. (Such continuations could
     potentially violate the LR invariant and lead to crashes.) *)

  type 'a result =
    | InputNeeded of env
    | Shifting of env * env * bool
    | AboutToReduce of env * production
    | HandlingError of env
    | Accepted of 'a
    | Rejected

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

  (* The following recursive group of functions are tail recursive, produce a
     result of type [semantic_value result], and cannot raise an exception. A
     semantic action can raise [Error], but this exception is immediately
     caught within [reduce]. *)

  let rec run env please_discard : semantic_value result =

    (* Log the fact that we just entered this state. *)
    
    if log then
      Log.state env.current;

    (* If [please_discard] is set, we discard the current lookahead token and
       fetch the next one. In order to request a token from the user, we
       return an [InputNeeded] continuation, which, when invoked by the user,
       will take us to [discard]. If [please_discard] is not set, we skip this
       step and jump directly to [check_for_default_reduction]. *)

    if please_discard then
      InputNeeded env
    else
      check_for_default_reduction env

  (* [discard env triple] stores [triple] into [env], overwriting the previous
     token. It is invoked by [offer], which itself is invoked by the user in
     response to an [InputNeeded] result. *)

  and discard env triple =
    if log then begin
      let (token, startp, endp) = triple in
      Log.lookahead_token (T.token2terminal token) startp endp
    end;
    let env = { env with error = false; triple } in
    check_for_default_reduction env

  and check_for_default_reduction env =

    (* Examine what situation we are in. This case analysis is analogous to
       that performed in [CodeBackend.gettoken], in the sub-case where we do
       not have a terminal incoming symbol. *)

    T.default_reduction
      env.current
      announce_reduce       (* there is a default reduction; perform it *)
      check_for_error_token (* there is none; continue below *)
      env

  and check_for_error_token env =

    (* There is no default reduction. Consult the current lookahead token
       so as to determine which action should be taken. *)

    (* Peeking at the first input token, without taking it off the input
       stream, is done by reading [env.triple]. We are careful to first
       check [env.error]. *)

    (* Note that, if [please_discard] was true, then we have just called
       [discard], so the lookahead token cannot be [error]. *)

    (* Returning [HandlingError env] is equivalent to calling [error env]
       directly, except it allows the user to regain control. *)

    if env.error then begin
      if log then
        Log.resuming_error_handling();
      HandlingError env
    end
    else
      let (token, _, _) = env.triple in

      (* We consult the two-dimensional action table, indexed by the
         current state and the current lookahead token, in order to
         determine which action should be taken. *)

      T.action
        env.current                    (* determines a row *)
        (T.token2terminal token)       (* determines a column *)
        (T.token2value token)
        shift                          (* shift continuation *)
        announce_reduce                (* reduce continuation *)
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
      (s' : state) =

    (* Log the transition. *)

    if log then
      Log.shift terminal s';

    (* Push a new cell onto the stack, containing the identity of the
       state that we are leaving. *)

    let (_, startp, endp) = env.triple in
    let stack = {
      state = env.current;
      semv = value;
      startp;
      endp;
      next = env.stack;
    } in

    (* Switch to state [s']. *)

    let new_env = { env with stack; current = s' } in

    (* Expose the transition to the user. (In principle, we have a choice
       between exposing the transition before we take it, after we take
       it, or at some point in between. This affects the number and type
       of the parameters carried by [Shifting]. Here, we choose to expose
       the transition after we take it; this allows [Shifting] to carry
       only three parameters, whose meaning is simple.) *)

    Shifting (env, new_env, please_discard)

  (* --------------------------------------------------------------------------- *)

  (* The function [announce_reduce] stops the parser and returns a result
     which allows the parser to be resumed by calling [reduce]. *)

  (* Only ordinary productions are exposed to the user. Start productions
     are not exposed to the user. Reducing a start production simply leads
     to the successful termination of the parser. *)

  and announce_reduce env (prod : production) =
    if T.is_start prod then
      accept env prod
    else
      AboutToReduce (env, prod)

  (* The function [reduce] takes care of reductions. It is invoked by
     [resume] after an [AboutToReduce] event has been produced. *)

  (* Here, the lookahead token CAN be [error]. *)

  (* The production [prod] CANNOT be a start production. *)

  and reduce env (prod : production) =

    (* Log a reduction event. *)

    if log then
      Log.reduce_or_accept prod;

    (* Invoke the semantic action. The semantic action is responsible for
       truncating the stack and pushing a new cell onto the stack, which
       contains a new semantic value. It can raise [Error]. *)

    (* If the semantic action terminates normally, it returns a new stack,
       which becomes the current stack. *)

    (* If the semantic action raises [Error], we catch it and initiate error
       handling. *)

    (* This [match/with/exception] construct requires OCaml 4.02. *)

    match T.semantic_action prod env with
    | stack ->

        (* By our convention, the semantic action has produced an updated
           stack. The state now found in the top stack cell is the return
           state. *)

        (* Perform a goto transition. The target state is determined
           by consulting the goto table at the return state and at
           production [prod]. *)

        let current = T.goto stack.state prod in
        let env = { env with stack; current } in
        run env false

    | exception Error ->
        initiate env

  and accept env prod =
    (* Log an accept event. *)
    if log then
      Log.reduce_or_accept prod;
    (* Extract the semantic value out of the stack. *)
    let v = env.stack.semv in
    (* Finish. *)
    Accepted v

  (* --------------------------------------------------------------------------- *)

  (* The following functions deal with errors. *)

  (* [initiate] initiates or resumes error handling. *)

  (* Here, the lookahead token CAN be [error]. *)

  and initiate env =
    Log.initiating_error_handling();
    let env = { env with error = true } in
    HandlingError env

  (* [error] handles errors. *)

  and error env =
    assert env.error;

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
      (* Intentionally calling [reduce] instead of [announce_reduce].
         It does not seem very useful, and it could be confusing, to
         expose the reduction steps taken during error handling. *)

  and error_fail env =

    (* This state is unable to handle errors. Attempt to pop a stack
       cell. *)

    let cell = env.stack in
    let next = cell.next in
    if next == cell then

      (* The stack is empty. Die. *)

      Rejected

    else begin

      (* The stack is nonempty. Pop a cell, updating the current state
	 with that found in the popped cell, and try again. *)

      let env = { env with
        stack = next;
        current = cell.state
      } in
      HandlingError env

    end

  (* End of the nest of tail recursive functions. *)

  (* --------------------------------------------------------------------------- *)
  (* --------------------------------------------------------------------------- *)

  (* The incremental interface. See [EngineTypes]. *)

  (* [start s] begins the parsing process. *)

  let start (s : state) : semantic_value result =
    
    (* Build an empty stack. This is a dummy cell, which is its own
       successor. Its fields other than [next] contain dummy values.
       Its [next] field WILL be accessed by [error_fail] if an error
       occurs and is propagated all the way until the stack is empty. *)

    let rec empty = {
      state = s;                          (* dummy *)
      semv = T.error_value;               (* dummy *)
      startp = Lexing.dummy_pos;          (* dummy *)
      endp = Lexing.dummy_pos;            (* dummy *)
      next = empty;
    } in

    (* Build an initial environment. *)

    (* Unfortunately, there is no type-safe way of constructing a
       dummy token. Tokens carry semantic values, which in general
       we cannot manufacture. This instance of [Obj.magic] could
       be avoided by adopting a different representation (e.g., no
       [env.error] field, and an option in the first component of
       [env.triple]), but I like this representation better. *)

    let dummy_token = Obj.magic () in
    let env = {
      error = false;
      triple = (dummy_token, Lexing.dummy_pos, Lexing.dummy_pos); (* dummy *)
      stack = empty;
      current = s;
    } in

    (* Begin parsing. *)

    (* The parameter [please_discard] here is [true], which means we know
       that we must read at least one token. This claim relies on the fact
       that we have ruled out the two special cases where a start symbol
       recognizes the empty language or the singleton language {epsilon}. *)

    run env true

  (* [offer result triple] is invoked by the user in response to a result
     of the form [InputNeeded env]. It checks that [result] is indeed of
     this form, and invokes [discard]. *)

  (* [resume result] is invoked by the user in response to a result of the
     form [AboutToReduce (env, prod)] or [HandlingError env]. It checks
     that [result] is indeed of this form, and invokes [reduce] or [error],
     as appropriate. *)

  (* In reality, [offer] and [resume] accept an argument of type
     [semantic_value result] and produce a result of the same type. The choice
     of [semantic_value] is forced by the fact that this is the parameter of
     the result [Accepted]. *)

  (* We change this as follows. *)

  (* We change the argument and result type of [offer] and [resume] from
     [semantic_value result] to ['a result]. This is safe, in this case,
     because we give the user access to values of type [t result] only if [t]
     is indeed the type of the eventual semantic value for this run. (More
     precisely, by examining the signatures [INCREMENTAL_ENGINE] and
     [INCREMENTAL_ENGINE_START], one finds that the user can build a value of
     type ['a result] only if ['a] is [semantic_value]. The table back-end
     goes further than this and produces versions of [start] composed with a
     suitable cast, which give the user access to a value of type [t result]
     where [t] is the type of the start symbol.) *)

  let offer : 'a . 'a result -> token * Lexing.position * Lexing.position -> 'a result = function
    | InputNeeded env ->
        Obj.magic discard env
    | _ ->
        raise (Invalid_argument "offer expects InputNeeded")

  let resume : 'a . 'a result -> 'a result = function
    | HandlingError env ->
        Obj.magic error env
    | Shifting (_, env, please_discard) ->
        Obj.magic run env please_discard
    | AboutToReduce (env, prod) ->
        Obj.magic reduce env prod
    | _ ->
        raise (Invalid_argument "resume expects HandlingError | AboutToReduce")

  (* --------------------------------------------------------------------------- *)
  (* --------------------------------------------------------------------------- *)

  (* The traditional interface. See [EngineTypes]. *)

  (* --------------------------------------------------------------------------- *)

  (* Wrapping a lexer and lexbuf as a reader. *)

  type reader =
    unit -> token * Lexing.position * Lexing.position

  let wrap (lexer : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) : reader =
    fun () ->
      let token = lexer lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.Lexing.lex_curr_p in
      token, startp, endp

  (* --------------------------------------------------------------------------- *)

  (* The main loop repeatedly handles intermediate results, until a final result
     is obtained. This allows implementing the monolithic interface ([entry]) in
     terms of the incremental interface ([start], [offer], [handle], [reduce]). *)

  (* By convention, acceptance is reported by returning a semantic value, whereas
     rejection is reported by raising [Error]. *)

  (* [loop] is polymorphic in ['a]. No cheating is involved in achieving this.
     All of the cheating resides in the types assigned to [offer] and [handle]
     above. *)

  let rec loop : 'a . reader -> 'a result -> 'a =
    fun read result ->
    match result with
    | InputNeeded _ ->
        (* The parser needs a token. Request one from the lexer,
           and offer it to the parser, which will produce a new
           result. Then, repeat. *)
        let triple = read() in
        let result = offer result triple in
        loop read result
    | Shifting _
    | AboutToReduce _
    | HandlingError _ ->
        (* The parser has suspended itself, but does not need
           new input. Just resume the parser. Then, repeat. *)
        let result = resume result in
        loop read result
    | Accepted v ->
        (* The parser has succeeded and produced a semantic value.
           Return this semantic value to the user. *)
        v
    | Rejected ->
        (* The parser rejects this input. Raise an exception. *)
        raise Error

  let entry (s : state) lexer lexbuf : semantic_value =
    loop (wrap lexer lexbuf) (start s)

  (* --------------------------------------------------------------------------- *)

  (* The type ['a lr1state] describes the (non-initial) states of the LR(1)
     automaton. The index ['a] represents the type of the semantic value
     associated with the state's incoming symbol. *)

  (* The type ['a lr1state] is defined as an alias for [state], which itself
     is usually defined as [int] (see [TableInterpreter]). So, ['a lr1state]
     is technically a phantom type, but should really be thought of as a GADT
     whose data constructors happen to be represented as integers. It is
     presented to the user as an abstract type (see [IncrementalEngine]). *)

  type 'a lr1state =
      state

  (* --------------------------------------------------------------------------- *)

  (* Stack inspection. *)

  (* We offer a read-only view of the parser's state as a stream of elements.
     Each element contains a pair of a (non-initial) state and a semantic
     value associated with (the incoming symbol of) this state. Note that the
     type [element] is an existential type. *)

  type element =
    | Element: 'a lr1state * 'a * Lexing.position * Lexing.position -> element

  open General

  type stack =
    element stream

  (* If [current] is the current state and [cell] is the top stack cell,
     then [stack cell current] is a view of the parser's state as a stream
     of elements. *)

  let rec stack cell current : element stream =
    lazy (
      (* The stack is empty iff the top stack cell is its own successor. In
         that case, the current state [current] should be an initial state
         (which has no incoming symbol).
         We do not allow the user to inspect this state. *)
      let next = cell.next in
      if next == cell then
        Nil
      else
        (* Construct an element containing the current state [current] as well
           as the semantic value contained in the top stack cell. This semantic
           value is associated with the incoming symbol of this state, so it
           makes sense to pair them together. The state has type ['a state] and
           the semantic value has type ['a], for some type ['a]. Here, the OCaml
           type-checker thinks ['a] is [semantic_value] and considers this code
           well-typed. Outside, we will use magic to provide the user with a way
           of inspecting states and recovering the value of ['a]. *)
        let element = Element (
          current,
          cell.semv,
          cell.startp,
          cell.endp
        ) in
        Cons (element, stack next cell.state)
    )

  let stack env : element stream =
    stack env.stack env.current

end

