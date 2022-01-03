open Printf

let () =
  let lexbuf = Lexing.from_string "FOO" in
  match Parser.main Lexer.read lexbuf with
  | Syntax.Foo ->
      printf "Got FOO as expected.\n%!"
  | Syntax.Bar ->
      assert false
