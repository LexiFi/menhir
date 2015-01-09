(* This functor is invoked inside the generated parser, in [--table] mode. It
   produces no code! It simply constructs the types [symbol] and [xsymbol] on
   top of the generated types [terminal] and [nonterminal]. *)

module Symbols (T : sig

  type 'a terminal
  type 'a nonterminal

end)

: IncrementalEngine.SYMBOLS
  with type 'a terminal := 'a T.terminal
   and type 'a nonterminal := 'a T.nonterminal

(* This functor is invoked inside the generated parser, in [--table] mode. It
   constructs the inspection API on top of the inspection tables described in
   [InspectionTableFormat]. *)

module Make (T : InspectionTableFormat.TABLES
             with type 'a lr1state = int)

: IncrementalEngine.INSPECTION
  with type 'a terminal := 'a T.terminal
   and type 'a nonterminal := 'a T.nonterminal
   and type 'a lr1state := 'a T.lr1state
   and type production := int

