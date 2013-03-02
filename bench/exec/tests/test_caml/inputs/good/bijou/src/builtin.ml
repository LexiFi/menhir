open Tokens
open Source.Raw

let defs = [
  CMPEQ
]

let defs =
  List.map (fun f -> DefBuiltin (LexerUtil.builtin2id f)) defs

