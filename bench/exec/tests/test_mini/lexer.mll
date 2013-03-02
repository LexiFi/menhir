{
  open @INCLUDE_PARSER@
  exception Eof
}

let blank = [ ' ' '\t' '\n']

let a = 'a'
let b = 'b'
let c = 'c'
let d = 'd'
let e = 'e'

rule token = parse
   a { A }
 | b { B }
 | c { C }
 | d { D }
 | e { E }
 | eof { raise Eof }
 | blank { token lexbuf }

 {
let lexbuf = Lexing.from_channel stdin in
try
  while true do
    let () = @INCLUDE_PARSER@.main token lexbuf in
    ()
  done
with Eof -> exit 0
 }

