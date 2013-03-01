let () =
  Pprint.Channel.pretty 1.0 80 stdout (
    PrintIL.structure_items (
      Sorts.defs @
      Types.defs
    )
  );
  print_newline();
  flush stdout

