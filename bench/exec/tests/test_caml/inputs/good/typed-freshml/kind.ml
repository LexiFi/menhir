open Source

let equal k1 k2 =
  match k1, k2 with
  | KExpression, KExpression
  | KPattern, KPattern ->
      true
  | _, _ ->
      false

let print = function
  | KExpression ->
      "an expression type"
  | KPattern ->
      "a pattern type"

