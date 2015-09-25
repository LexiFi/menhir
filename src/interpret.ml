(* This module is in charge of handling the [--interpret] option,
   if it is present. *)

module I = Invariant (* artificial dependency; ensures that [Invariant] runs first *)

(* --------------------------------------------------------------------------- *)

(* The following definitions are in sync with [SentenceParser]. *)

open Grammar
type terminals = Terminal.t list
type sentence = Nonterminal.t option * terminals
type located_sentence = Positions.positions * sentence
type message = string

(* A run is a series of sentences together with an error message. *)

type run = located_sentence list * message

(* A targeted sentence is a located sentence together with the state
   into which it leads. *)

type targeted_sentence = located_sentence * Lr1.node

(* A targeted run is a series of targeted sentences together with an error
   message. *)

type targeted_run = targeted_sentence list * message

(* --------------------------------------------------------------------------- *)

(* Debugging.

let print_sentence (nto, terminals) : string =
  let b = Buffer.create 128 in
  Option.iter (fun nt ->
    Printf.bprintf b "%s: " (Nonterminal.print true nt)
  ) nto;
  List.iter (fun t ->
    Printf.bprintf b "%s " (Terminal.print t)
  ) terminals;
  Printf.bprintf b "\n";
  Buffer.contents b

let print_sentence sentence : unit =
  print_string (print_sentence sentence)

let print_located_sentence (_, sentence) : unit =
  print_sentence sentence

*)

(* --------------------------------------------------------------------------- *)

(* [stream] turns a finite list of terminals into a stream of terminals. *)

exception EndOfStream

let stream (toks : Terminal.t list) : unit -> Terminal.t * Lexing.position * Lexing.position =
  let toks = ref toks in
  fun () ->

    let tok =
      match !toks with
      | tok :: more ->

	  (* Take a token off the list, and return it. *)

	  toks := more;
	  tok

      | [] ->

	  (* The finite list has been exhausted. Here, two plausible behaviors
	     come to mind.

	     The first behavior consists in raising an exception. In that case,
	     we are creating a finite stream, and it is up to the parser to not
	     read past its end.

	     The second behavior consists in returning a designated token. In
	     that case, we are creating an infinite, eventually constant,
	     stream.

	     The choice between these two behaviors is somewhat arbitrary;
	     furthermore, in the second case, the choice of the designated
	     token is arbitrary as well. Here, we adopt the second behavior if
	     and only if the grammar has an EOF token, and we use EOF as the
	     designated token. Again, this is arbitrary, and could be changed
	     in the future. *)

	  match Terminal.eof with
	  | Some eof ->
	      eof
	  | None ->
	      raise EndOfStream

    in

    (* For now, return dummy positions. *)

    tok, Lexing.dummy_pos, Lexing.dummy_pos

(* --------------------------------------------------------------------------- *)

(* [start sentence] returns the start symbol that we should use to interpret
   the sentence [sentence]. *)

(* If a start symbol was explicitly provided as part of the sentence, we use
   it. Otherwise, we use the grammar's unique start symbol, if there is
   one. *)

let start poss ((nto, _) : sentence) : Nonterminal.t =
  match nto with
  | Some nt ->
      nt
  | None ->
      match ProductionMap.is_singleton Lr1.entry with
      | None ->
          Error.error poss
            "Because the grammar has multiple start symbols, each of the\n\
             sentences provided on the standard input channel must be of the\n\
             form: <start symbol>: <token>*"
      | Some (prod, _) ->
          match Production.classify prod with
          | Some nt ->
              nt
          | None ->
              assert false

(* --------------------------------------------------------------------------- *)

(* [interpret] interprets a sentence. *)

let interpret ((_, toks) as sentence) : unit =

  let nt = start [] sentence in

  (* Run the reference interpreter. This can produce a concrete syntax tree
     ([Some cst]), fail with a parser error ([None]), or fail with a lexer error
     ([EndOfStream]). *)

  (* In either case, we produce just one line of output, so it should be clear
     to the user which outcomes correspond to which sentences (should multiple
     sentences be supplied). *)

  begin try
    match
      MenhirLib.Convert.Simplified.traditional2revised
	(ReferenceInterpreter.interpret Settings.trace nt)
	(stream toks)
    with

    | Some cst ->

	(* Success. *)

	Printf.printf "ACCEPT";
	if Settings.interpret_show_cst then begin
	  print_newline();
	  Cst.show stdout cst
	end

    | None ->

	(* Parser failure. *)

	Printf.printf "REJECT"

  with EndOfStream ->

    (* Lexer failure. *)
    
    Printf.printf "OVERSHOOT"

  end;
  print_newline()

(* --------------------------------------------------------------------------- *)

(* [interpret_error_aux] interprets a sentence, expecting it to end in an
   error. Failure or success is reported via two continuations. *)

let interpret_error_aux poss ((_, terminals) as sentence) fail succeed =
  let nt = start poss sentence in
  let open ReferenceInterpreter in
  match check_error_path nt terminals with
  | OInputReadPastEnd ->
      fail "No syntax error occurs."
  | OInputNotFullyConsumed ->
      fail "A syntax error occurs before the last token is reached."
  | OUnexpectedAccept ->
      fail "No syntax error occurs; in fact, this input is accepted."
  | OK state ->
      succeed state

(* --------------------------------------------------------------------------- *)

(* [interpret_error] interprets a sentence, expecting it to end in an error.
   Failure or success is reported on the standard output channel. This is
   used by [--interpret-error]. *)

let fail msg =
  Printf.printf "BAD\n# %s\n%!" msg;
  exit 1

let succeed s =
  let s = Lr1.number s in
  Printf.printf
    "OK %d\n# This sentence ends with a syntax error in state %d.\n%!"
    s s;
  exit 0

let interpret_error sentence =
  interpret_error_aux [] sentence fail succeed

(* --------------------------------------------------------------------------- *)

(* [target_sentence] interprets a (located) sentence, expecting it to end in
   an error, computes the state in which the error is obtained, and constructs
   a targeted sentence. *)

let fail poss msg =
  Error.signal poss (Printf.sprintf
    "This sentence does not end with a syntax error, as desired.\n%s"
    msg
  );
  [] (* dummy result *)

let target_sentence : located_sentence -> targeted_sentence list =
  fun (poss, sentence) ->
    interpret_error_aux poss sentence
      (fail poss)
      (fun s -> [ (poss, sentence), s ])

let target_run : run -> targeted_run =
  fun (sentences, message) ->
    List.flatten (List.map target_sentence sentences), message

let target_runs : run list -> targeted_run list =
  fun runs ->
    let runs = List.map target_run runs in
    if Error.errors() then exit 1;
    runs

(* --------------------------------------------------------------------------- *)

(* [setup()] returns a function [read] which reads one sentence from the
   standard input channel. *)

let setup () : unit -> sentence option =

  let open Lexing in
  let lexbuf = from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };

  let read () =
    try
      SentenceParser.optional_sentence SentenceLexer.lex lexbuf
    with Parsing.Parse_error ->
      Error.error (Positions.lexbuf lexbuf) "Ill-formed input sentence."
  in

  read

(* --------------------------------------------------------------------------- *)

(* If [--interpret] is set, interpret the sentences found on the standard
   input channel, then stop, without generating a parser. *)

(* We read a series of sentences from the standard input channel. To allow
   interactive use, we interpret each sentence as soon as it is read. *)

let () =
  if Settings.interpret then
    let read = setup() in
    while true do
      match read() with
      | None ->
  	  exit 0
      | Some sentence ->
	  interpret sentence
    done

(* --------------------------------------------------------------------------- *)

(* If [--interpret-error] is set, interpret one sentence found on the standard
   input channel, then stop, without generating a parser. *)

(* We read just one sentence, confirm that this sentence ends in an error, and
   (if that is the case) display the number of the state that is reached. *)

let () =
  if Settings.interpret_error then
    let read = setup() in
    match read() with
    | None ->
      exit 1 (* abnormal: no input *)
    | Some sentence ->
        interpret_error sentence (* never returns *)

(* --------------------------------------------------------------------------- *)

(* Reading a [.messages] file. *)

let read_messages filename : run list =
  (* Read and segment the file. *)
  let segments : (string * Lexing.lexbuf) list = Segment.segment filename in
  (* Process the segments, two by two. We expect one segment to contain
     a non-empty series of sentences, and the next segment to contain
     free-form text. *)
  let rec loop accu segments =
    match segments with
    | [] ->
        List.rev accu
    | (_, lexbuf) :: [] ->
        (* Oops, we are desynchronized. *)
        Error.signal
          (Positions.one (Lexing.lexeme_end_p lexbuf))
          "Syntax error: missing a final message. I may be desynchronized.";
        List.rev accu
    | (_, lexbuf) :: (text, _) :: segments ->
        (* Read a non-empty series of located sentences. *)
        match SentenceParser.entry SentenceLexer.lex lexbuf with
        | exception Parsing.Parse_error ->
            (* Report an error. *)
            Error.signal
              (Positions.one (Lexing.lexeme_start_p lexbuf))
              "Syntax error: ill-formed sentence.";
            (* Continue anyway. *)
            loop accu segments
        | sentences ->
            loop ((sentences, text) :: accu) segments
  in
  let runs = loop [] segments in
  if Error.errors() then exit 1;
  (* Although we try to report several errors, [SentenceLexer.lex] may
     abort the whole process after just one error. This could be improved. *)
  runs

(* --------------------------------------------------------------------------- *)

(* [message_table] converts a list of targeted runs to a table (a mapping) of
   states to located sentences and messages. Optionally, it can detect that
   two sentences lead to the same state, and report an error. *)

let message_table (detect_redundancy : bool) (runs : targeted_run list)
  : (located_sentence * message) Lr1.NodeMap.t =

  let table =
    List.fold_left (fun table (sentences_and_states, message) ->
      List.fold_left (fun table (sentence2, s) ->
        match Lr1.NodeMap.find s table with
        | sentence1, _ ->
            if detect_redundancy then
              Error.signal (fst sentence1 @ fst sentence2)
                (Printf.sprintf
                   "Redundancy: these sentences both cause an error in state %d."
                   (Lr1.number s));
            table
        | exception Not_found ->
            Lr1.NodeMap.add s (sentence2, message) table
      ) table sentences_and_states
    ) Lr1.NodeMap.empty runs
  in
  if Error.errors() then exit 1;
  table

(* --------------------------------------------------------------------------- *)

(* [compile_runs] converts a list of targeted runs to OCaml code that encodes
   a mapping of state numbers to error messages. The code is sent to the
   standard output channel. *)

let compile_runs filename (runs : targeted_run list) : unit =

  (* We wish to produce a function that maps a state number to a message.
     By convention, we call this function [message]. *)

  let name = "message" in

  let open IL in
  let open CodeBits in
  let default = {
    branchpat  = PWildcard;
    branchbody = eraisenotfound
  (* The default branch raises an exception, which can be caught by
     the user, who can then produce a generic error message. *)
  } in
  let branches =
    List.fold_left (fun branches (sentences_and_states, message) ->
      (* Create an or-pattern for these states. *)
      let states = List.map (fun (_, s) ->
        pint (Lr1.number s)
      ) sentences_and_states in
      (* Map all these states to this message. *)
      { branchpat = POr states;
        branchbody = EStringConst message } :: branches
    ) [ default ] runs
  in
  let messagedef = {
    valpublic = true;
    valpat = PVar name;
    valval = EFun ([ PVar "s" ], EMatch (EVar "s", branches))
  } in
  let program = [
    SIComment (Printf.sprintf
      "This file was auto-generated based on \"%s\"." filename);
    SIComment (Printf.sprintf
      "Please note that the function [%s] can raise [Not_found]." name);
    SIValDefs (false,
      [ messagedef ]);
  ] in

  (* Write this program to the standard output channel. *)

  let module P = Printer.Make (struct
    let f = stdout
    let locate_stretches = None
  end) in
  P.program program

(* --------------------------------------------------------------------------- *)

(* If [--compile-errors <filename>] is set, compile the error message
   descriptions found in file [filename] down to OCaml code, then stop. *)

let () =
  Settings.compile_errors |> Option.iter (fun filename ->

    (* Read the file. *)
    let runs = read_messages filename in

    (* Convert every sentence to a state number. We signal an error if a
       sentence does not end in an error, as expected. *)
    let runs = target_runs runs in

    (* Build a mapping of states to located sentences. This allows us to
       detect if two sentences lead to the same state. *)
    let _ = message_table true runs in

    (* In principle, we would like to check whether this set of sentences is
       complete (i.e., covers all states where an error can arise), but this
       may be costly -- it requires running [LRijkstra]. Instead, we offer a
       separate facility for comparing two [.messages] files, one of which can
       be produced via [--list-errors]. This can be used to ensure
       completeness. *)

    (* Now, compile this information down to OCaml code. We wish to
       produce a function that maps a state number to a message. By
       convention, we call this function [message]. *)
    compile_runs filename runs;

    exit 0
  )

(* --------------------------------------------------------------------------- *)

(* If two [--compare-errors <filename>] directives are provided, compare the
   two message descriptions files, and stop. We wish to make sure that every
   state that appears on the left-hand side appears on the right-hand side as
   well. *)

let () =
  Settings.compare_errors |> Option.iter (fun (filename1, filename2) ->

    (* Read and convert both files, as above. *)
    let runs1 = read_messages filename1
    and runs2 = read_messages filename2 in
    let runs1 = target_runs runs1
    and runs2 = target_runs runs2 in (* here, it would be OK to ignore errors *)
    let table1 = message_table false runs1
    and table2 = message_table false runs2 in
    
    (* Check that the domain of [table1] is a subset of the domain of [table2]. *)
    table1 |> Lr1.NodeMap.iter (fun s ((poss1, _), _) ->
      if not (Lr1.NodeMap.mem s table2) then
        Error.signal poss1 (Printf.sprintf
          "This sentence leads to an error in state %d.\n\
           No sentence that leads to this state exists in \"%s\"."
          (Lr1.number s) filename2
        )
    );

    (* Check that [table1] is a subset of [table2], that is, for every state
       [s] in the domain of [table1], [s] is mapped by [table1] and [table2]
       to the same error message. *)
    table1 |> Lr1.NodeMap.iter (fun s ((poss1, _), message1) ->
      let (poss2, _), message2 = Lr1.NodeMap.find s table2 in
      if message1 <> message2 then
        Error.warning (poss1 @ poss2) (Printf.sprintf
          "These sentences lead to an error in state %d.\n\
           The corresponding messages in \"%s\" and \"%s\" differ."
          (Lr1.number s) filename1 filename2
        )
    );

    if Error.errors() then exit 1;
    exit 0

  )

