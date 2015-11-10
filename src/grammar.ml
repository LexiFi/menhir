(* This module runs the grammar functor on the grammar produced by the
   front-end. *)

include GrammarFunctor.Make(struct
  let grammar = Front.grammar
  let verbose = true
end)

