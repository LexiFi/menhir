type diff = Correct of int | Incorrect of string

val add : (GeneratedParser.Generated.t * GeneratedParser.Generated.t)  -> Inputs.Input.t -> diff -> unit
