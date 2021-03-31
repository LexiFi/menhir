let run () =
  if Settings.unit_test then (
    Array.test () ;
    StackLangUtils.test () ;
    StackLangTransform.test () ;
    exit 0
  )