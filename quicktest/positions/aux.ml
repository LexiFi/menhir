open Lexing

type positions =
  position * position

type nothing =
  positions

type optional_dot =
  positions * nothing option

type optional_comma =
  positions * nothing option

type annotations =
  positions * optional_dot * optional_comma

type raw_expr =
  | EInt
  | EParen of annotations * nothing * expr * optional_dot
  | EBinOp of expr * expr
  | EUnOp of expr

and expr =
  positions * raw_expr

type main =
  positions * nothing * expr

let iter f = function
  | None ->
      ()
  | Some x ->
      f x

module Print = struct

  let positions nt (startpos, endpos) =
    Printf.printf "%s: startpos = %s/%03d,\n%s:   endpos = %s/%03d\n"
      nt
      startpos.pos_fname
      startpos.pos_cnum
      nt
      endpos.pos_fname
      endpos.pos_cnum

  let nothing =
    positions "nothing"

  let optional_dot (poss, no) =
    positions "optional_dot" poss;
    iter nothing no

  let optional_comma (poss, no) =
    positions "optional_comma" poss;
    iter nothing no

  let annotations (poss, odot, ocomma) =
    positions "annotations" poss;
    optional_dot odot;
    optional_comma ocomma

  let rec raw_expr = function
    | EInt ->
        ()
    | EParen (a, n, e, o) ->
        annotations a;
        nothing n;
        expr e;
        optional_dot o
    | EBinOp (e1, e2) ->
        expr e1;
        expr e2
    | EUnOp e ->
        expr e

  and expr (poss, e) =
    positions "expr" poss;
    raw_expr e

  let main (poss, n, e) =
    positions "main" poss;
    nothing n;
    expr e

end

