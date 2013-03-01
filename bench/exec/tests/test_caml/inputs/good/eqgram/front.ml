(* This module reads in the grammar specification and produces an internal
   view of it. *)

open Printf
open Print
open AbstractSyntax

(* ------------------------------------------------------------------------- *)

(* Read the file and parse the program. *)

let grammar : grammar =
  let channel = open_in Settings.filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <-
      { 
	Lexing.pos_fname = Settings.filename; 
	Lexing.pos_lnum  = 1;
	Lexing.pos_bol   = 0; 
	Lexing.pos_cnum  = 0
      };
  try
    let grammar = Parser.grammar Lexer.main lexbuf in
    close_in channel;
    grammar
  with Parser.Error ->
    Error.error2
      (Lexing.lexeme_start_p lexbuf)
      (Lexing.lexeme_end_p lexbuf)
      "syntax error."

(* ------------------------------------------------------------------------- *)

(* Turn the grammar specification into a palatable internal form.
   First, index all symbols. *)

module StringMap =
  Map.Make (String)

let index (ids : identifier list) : int * identifier array * int StringMap.t =
  let name = Array.of_list ids
  and n, map = List.fold_left (fun (n, map) (p1, p2, s) ->
    if StringMap.mem s map then
      Error.error2 p1 p2 (sprintf "duplicate symbol: %s." s)
    else
      n+1, StringMap.add s n map
  ) (0, StringMap.empty) ids in
  n, name, map

let n, name, map =
  index (grammar.terminals @ grammar.nonterminals)

let nterminals =
  List.length grammar.terminals

let ntbase =
  nterminals

let nnonterminals =
  n - ntbase

let is_terminal s =
  assert (s >= 0 && s < n);
  s < ntbase

let is_nonterminal s =
  assert (s >= 0 && s < n);
  s >= ntbase

let iter_terminals f =
  for i = 0 to nterminals-1 do
    f i
  done

let iter_nonterminals f =
  for i = ntbase to n-1 do
    f i
  done

(* ------------------------------------------------------------------------- *)

(* Define the abstract type of symbols. *)

module Symbol = struct

  type symbol =
      int

  type t =
      symbol

  let equal (s1 : symbol) (s2 : symbol) =
    s1 = s2

  let compare (s1 : symbol) (s2 : symbol) =
    s1 - s2

  let display (s : symbol) =
    let _, _, id = name.(s) in
    id

  let print b (s : symbol) =
    Buffer.add_string b (display s)

  module Set = struct

    include CompressedBitSet

    let print b s =
      seplist space print b (elements s)

  end

  module Map =
    Map.Make (struct
      type t = symbol
      let compare = (-)
    end)

end

(* ------------------------------------------------------------------------- *)

(* Define words out of symbols. *)

module Word =
  ArrayWord.Make (Symbol) (* TEMPORARY experiment with both arrays and lists *)

(* ------------------------------------------------------------------------- *)

(* Internalize the grammar's productions. *)

let lookup ((p1, p2, s) : identifier) : Symbol.symbol =
  try
    StringMap.find s map
  with Not_found ->
    Error.error2 p1 p2 (sprintf "unknown symbol: %s." s)

let lookup_word (ss : identifier list) : Word.word =
  Word.construct (List.map lookup ss)

let productions : Word.word list array =
  Array.make nnonterminals []

let () =
  List.iter (fun prod ->
    let lhs = lookup prod.lhs
    and rhs = lookup_word prod.rhs in
    productions.(lhs - ntbase) <- rhs :: productions.(lhs - ntbase)
  ) grammar.productions

let print_production b (lhs, rhs) =
  bprintf b "%a -> %a" Symbol.print lhs Word.print rhs

let () =
  if false then (* toggle to display the grammar's productions *)
    Array.iteri (fun lhs rhss ->
      let lhs = lhs + ntbase in
      List.iter (fun rhs ->
	w (fun b ->
	  bprintf b "%a\n%!" print_production (lhs, rhs)
        )
      ) rhss
    ) productions

(* ------------------------------------------------------------------------- *)

(* Internalize the goals. *)

let goals =
  List.map (fun (lhs, rhs) ->
    lookup_word lhs, lookup_word rhs
  ) grammar.goals

(* ------------------------------------------------------------------------- *)

(* Build the grammar's backward reference graph. In this graph, edges
   relate each symbol [s] to each of the nonterminal symbols whose
   definition mentions [s]. The reverse reference graph is used in the
   computation of several properties of the grammar. *)

let backward : Symbol.Set.t array =
  Array.make n Symbol.Set.empty

let () =
  Array.iteri (fun lhs rhss ->
    let lhs = lhs + ntbase in
    List.iter (fun rhs ->
      Word.iter (fun s ->
	backward.(s) <- Symbol.Set.add lhs backward.(s)
      ) rhs
    ) rhss
  ) productions

let print_symbol_set b ss =
  seplist comma Symbol.print b (Symbol.Set.elements ss)

let () =
  if false then (* toggle to display the backward reference graph *)
    Array.iteri (fun s ss ->
      w (fun b ->
	bprintf b "%a is referred to by %a\n" Symbol.print s print_symbol_set ss
      )
    ) backward

(* ------------------------------------------------------------------------ *)

(* Define backward fixpoint computations of properties of nonterminal
   symbols. *)

(* A fixpoint computation associates a property with every
   nonterminal. A number of monotone functions tell how properties are
   computed. More precisely, a property is defined by its behavior
   with respect to terminal symbols, void (the empty language),
   epsilon, sequence, choice, and expansion (of a non-terminal
   symbol). A bottom element is used to initialize the computation. An
   equality test is required in order to detect termination. *)

module type PROPERTY = sig

  type property

  val bottom: property (* TEMPORARY ever need bottom to be different from void? *)
  val terminal: Symbol.symbol -> property
  val void: property
  val epsilon: property
  val sequence: property -> property -> property
  val choice: property -> property -> property
  val expand: property -> property
  val equal: property -> property -> bool
  val print: property printer

end

module Fix (P : PROPERTY) : sig

  open P

  val get: Symbol.symbol -> property
  val get_word: Word.word -> property

end = struct

  open P

  (* [get_word get w] computes the property associated with the word
     [w] in terms of the properties currently associated via [get]
     with the symbols in [w]. *)

  let get_word get (w : Word.word) =
    Word.fold (fun s accu ->
      sequence accu (get s)
    ) w epsilon

  module Transfer = struct

    (* [transfer get s] computes the property associated with the
       symbol [s]. When [s] is a non-terminal symbol [nt], the new
       property is computed in terms of the properties currently
       associated with the symbols that appear in the definition of
       [nt]. *)

    let transfer get s =
      if is_terminal s then
	terminal s
      else
	List.fold_left (fun accu rhs ->
	  choice accu (expand (get_word get rhs))
	) void productions.(s - ntbase)

  end

  include FixOnDemand.Make (Symbol.Map) (P) (Transfer)

  let get_word w =
    get_word get w

end

(* ------------------------------------------------------------------------- *)

(* Reject the grammar if a nonterminal symbol produces the empty language. *)

module Nonempty = Fix (struct
  type property = bool
  let bottom = false
  let terminal _ = true
  let void = false
  let epsilon = true
  let sequence = (&&)
  let choice = (||)
  let expand p = p
  let equal = (=)
  let print b p = bprintf b "%b" p
end)

let () =
  iter_nonterminals (fun nt ->
    if not (Nonempty.get nt) then
      Error.signal (sprintf "the nonterminal symbol %s generates the empty language." (Symbol.display nt))
  );
  Error.check()

(* ------------------------------------------------------------------------- *)

(* Reject the grammar if a nonterminal symbol produces the singleton
   epsilon language. *)

module Epsilon = Fix (struct
  type property = bool
  let bottom = true
  let terminal _ = false
  let void = true
  let epsilon = true
  let sequence = (&&)
  let choice = (&&) (* because empty non-terminals have been ruled out above *)
  let expand p = p
  let equal = (=)
  let print b p = bprintf b "%b" p
end)

let () =
  iter_nonterminals (fun nt ->
    if Epsilon.get nt then
      Error.signal (sprintf "the nonterminal symbol %s generates the language { epsilon }." (Symbol.display nt))
  );
  Error.check()

(* ------------------------------------------------------------------------- *)

(* Compute FIRST, NULLABLE, LAST, and ALL properties. *)

module FirstNullableLastInfo = struct

  type property = {
    first: Symbol.Set.t;
    nullable: bool;
    last: Symbol.Set.t;
    all: Symbol.Set.t;
  }

  let terminal s = 
    let singleton = Symbol.Set.singleton s in
    {
      first = singleton;
      nullable = false;
      last = singleton;
      all = singleton;
    }

  let void = {
    first = Symbol.Set.empty;
    nullable = false;
    last = Symbol.Set.empty;
    all = Symbol.Set.empty;
  }

  let bottom =
    void
  
  let epsilon = {
    first = Symbol.Set.empty;
    nullable = true;
    last = Symbol.Set.empty;
    all = Symbol.Set.empty;
  }

  let sequence p1 p2 = {
    first =
      if p1.nullable then
	Symbol.Set.union p1.first p2.first
      else
	p1.first;
    nullable =
      p1.nullable && p2.nullable;
    last =
      if p2.nullable then
	Symbol.Set.union p1.last p2.last
      else
	p2.last;
    all =
      Symbol.Set.union p1.all p2.all;
  }

  let choice p1 p2 = {
    first = Symbol.Set.union p1.first p2.first;
    nullable = p1.nullable || p2.nullable;
    last = Symbol.Set.union p1.last p2.last;
    all = Symbol.Set.union p1.all p2.all;
  }

  let expand p =
    p

  let equal p1 p2 =
    p1.nullable = p2.nullable &&
    Symbol.Set.equal p1.first p2.first &&
    Symbol.Set.equal p1.last p2.last &&
    Symbol.Set.equal p1.all p2.all

  let print b p =
    bprintf b "{ FIRST = %a ; NULLABLE = %b ; LAST = %a ; ALL = %a }"
      Symbol.Set.print p.first
      p.nullable
      Symbol.Set.print p.last
      Symbol.Set.print p.all

end

module FirstNullableLast =
  Fix (FirstNullableLastInfo)

let () =
  if false then (* toggle to display FIRST, NULLABLE, LAST, and ALL info *)
    iter_nonterminals (fun nt ->
      w (fun b ->
	bprintf b "%a: %a\n"
	  Symbol.print nt
	  FirstNullableLastInfo.print (FirstNullableLast.get nt)
      )
    )

(* ------------------------------------------------------------------------- *)

(* Compute heights.

   The height of a word is the minimal number of steps required to
   rewrite it into a ground word (one that does not contain any
   transparent non-terminal symbols). *)

(* TEMPORARY unused? *)

module Height = Fix (struct

  type property =
      int

  let terminal _ =
    0

  let void =
    max_int

  let bottom =
    void

  let epsilon =
    0

  let sequence p1 p2 =
    if p1 = max_int then
      p1
    else if p2 = max_int then
      p2
    else
      p1 + p2

  let choice p1 p2 =
    min p1 p2

  let expand p =
    if p = max_int then
      p
    else
      p + 1 (* count expansions *)

  let equal (p1 : property) (p2 : property) =
    p1 = p2

  let print b p =
    bprintf b "%d" p

end)

let () =
  if false then (* toggle *)
    iter_nonterminals (fun nt ->
      w (fun b ->
	bprintf b "%a has height %d\n"
	  Symbol.print nt
	  (Height.get nt)
      )
    )

(* ------------------------------------------------------------------------- *)

(* Compute the strongly connected components of the grammars' reference graph
   and use it to determine which productions are decreasing. A decreasing
   production is one that replaces a non-terminal symbol [s] with a word whose
   symbols are strictly less than [s] in the pre-order represented by the
   reference graph. No infinite sequence of decreasing rewriting steps exists,
   which makes this notion interesting. *)

module DSCC = Tarjan.Run (struct

  type node =
      Symbol.symbol

  let n =
    nnonterminals

  let index s =
    s - ntbase

  let successors f s =
    Symbol.Set.iter f backward.(s)

  let iter =
    iter_nonterminals

end)

let productions : (bool * Word.word) list array =
  Array.mapi (fun lhs rhss ->
    let lhs = lhs + ntbase in
    List.map (fun rhs ->
      
      (* Determine whether rewriting [lhs] to [rhs] is a decreasing step. *)

      let decreasing =
	not (Word.exists (fun s ->
	  is_nonterminal s && DSCC.equivalent lhs s
	) rhs)
      in

      if false then (* toggle *)
	w (fun b ->
	  bprintf b "Production %a is %sdecreasing.\n"
	    print_production (lhs, rhs)
	    (if decreasing then "" else "non-")
	);

      decreasing, rhs

    ) rhss
  ) productions

(* ------------------------------------------------------------------------- *)

(* Publish a view of the grammar. *)

module Grammar = struct

  module Word = Word

  type classification =
    | T
    | NO of bool (* nullable *)
    | NT of (bool (* decreasing *) * Word.word) list

  let classify s =
    if is_terminal s then
      T
    else
      NT productions.(s - ntbase) (* TEMPORARY no opaque symbols for now *)

  open FirstNullableLastInfo
  open FirstNullableLast

  let nullable w =
    (get_word w).nullable

  let nullable_symbol s =
    (get s).nullable

  let abstract_inclusion w1 w2 =
    let p1 = get_word w1
    and p2 = get_word w2 in
    (if p1.nullable then p2.nullable else true) && (* if [w1] is nullable, [w2] must be nullable as well *)
    Symbol.Set.subset p1.first p2.first &&
    Symbol.Set.subset p1.last p2.last &&
    Symbol.Set.subset p1.all p2.all

  open Height

  let height w =
    get_word w

end

(* ------------------------------------------------------------------------- *)

(* TEMPORARY determine which nonterminals generate regular languages and
   precompute their inclusion relationships? *)

