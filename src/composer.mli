open EngineTypes

module Make (T : TABLE) : sig

  (* The composer is invoked with (1) the parsing engine's environment at the
     point where an error was encoutered; and (2) an integer fuel, which
     indicates how many terminal symbols (at most) the composer should
     produce. *)

  val entry:
    (T.state, T.semantic_value, T.token) env ->
    int ->
    suggestion

end
