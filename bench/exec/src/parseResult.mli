val set_time : GeneratedParser.Generated.t -> Inputs.Input.t -> BenchTypes.return -> float option -> unit
val set_trace : GeneratedParser.Generated.t -> Inputs.Input.t -> BenchTypes.return -> Traces.t option -> unit
val get_result : GeneratedParser.Generated.t -> Inputs.Input.t -> BenchTypes.return
val get_time : GeneratedParser.Generated.t -> Inputs.Input.t -> float option
val get_trace : GeneratedParser.Generated.t -> Inputs.Input.t -> Traces.t option
