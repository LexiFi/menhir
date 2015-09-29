open Grammar

type terminals =
  Terminal.t list

type sentence =
  Nonterminal.t option * terminals

type located_sentence =
  Positions.positions * sentence

type comment =
  string

type 'a or_comment =
| Sentence of 'a
| Comment of comment

let or_comment_map f = function
  | Sentence s ->
      Sentence (f s)
  | Comment c ->
      Comment c

let unSentence = function
  | Sentence x ->
      [ x ]
  | Comment _ ->
      []
