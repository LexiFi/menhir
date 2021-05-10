let run () =
  if Settings.unit_test then (
    print_endline "Starting unit tests" ;
    flush_all () ;
    MArray.test () ;
    StackLangUtils.test () ;
    (* StackLangTransform.test () ; *)
    StackLangTraverse.test () ;
    print_endline "Tests passed with success" ;
    flush_all () ;
    exit 0 )
