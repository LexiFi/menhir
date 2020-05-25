let () =
  let lexbuf = Lexing.from_string "T" in
  match Parser.main Lexer.read lexbuf with
  | `A s ->
      Printf.printf "A %s\n" s
  | `T i ->
      Printf.printf "T %d\n" i
