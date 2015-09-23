(* The purpose of this algorithm is to find, for each pair of a state [s]
   and a terminal symbol [z] such that looking at [z] in state [s] causes
   an error, a minimal path (starting in some initial state) that actually
   triggers this error. *)

(* The result of this analysis is written to the standard output channel.
   No result is returned. *)

module Run (X : sig
  (* If [verbose] is set, produce various messages on [stderr]. *)
  val verbose: bool
  (* If [statistics] is defined, it is interpreted as the name of
     a file to which one line of statistics is appended. *)
  val statistics: string option
end) : sig end

