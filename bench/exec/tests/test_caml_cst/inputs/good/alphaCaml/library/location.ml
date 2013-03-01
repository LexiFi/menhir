type location =
    Lexing.position * Lexing.position

let current : location ref =
  ref (Lexing.dummy_pos, Lexing.dummy_pos)

let under loc f x =
  let old = !current in
  current := loc;
  let y =
    try
      f x
    with e ->
      current := old;
      raise e
  in
  current := old;
  y

let current () =
  !current

