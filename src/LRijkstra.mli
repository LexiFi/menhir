(* The purpose of this algorithm is to find, for each pair of a state [s]
   and a terminal symbol [z] such that looking at [z] in state [s] causes
   an error, a minimal path (starting in some initial state) that actually
   triggers this error. *)

(* The output of this analysis is written to a .coverage file. No result
   is returned. *)

module Run (X : sig end) : sig end

