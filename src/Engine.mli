open EngineTypes

(* The LR parsing engine. *)

module Make (T : TABLE) : ENGINE with type state = T.state
                                  and type token = T.token
				  and type semantic_value = T.semantic_value
                                  and type production = T.production
