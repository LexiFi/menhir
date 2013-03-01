(* This module displays graphs in graphviz dot format. It is much more
   basic than the one bundled with the ocamlgraph library, but offers
   the advantage of being stand-alone. *)

(* ------------------------------------------------------------------------- *)

(* Type definitions. *)

type size =
    float * float (* in inches *)

type orientation =
  | Portrait
  | Landscape

type rankdir =
  | LeftToRight
  | TopToBottom

type ratio =
  | Compress
  | Fill
  | Auto

type style =

    (* Both nodes and edges. *)

  | Solid
  | Dashed
  | Dotted
  | Bold
  | Invisible

    (* Nodes only. *)

  | Filled
  | Diagonals
  | Rounded

(* ------------------------------------------------------------------------- *)

(* The graph printer. *)

module Print (G : sig

  type vertex

  val name: vertex -> string

  val successors: (?style:style -> label:string -> vertex -> unit) -> vertex -> unit

  val iter: (?style:style -> label:string -> vertex -> unit) -> unit

end) : sig

  val print:
      ?directed: bool ->
      ?size: size ->
      ?orientation: orientation ->
      ?rankdir: rankdir ->
      ?ratio: ratio ->
      out_channel ->
      unit

end

