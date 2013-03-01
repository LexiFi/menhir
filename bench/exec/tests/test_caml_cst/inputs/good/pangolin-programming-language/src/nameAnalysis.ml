(* $Id: nameAnalysis.ml 46 2007-10-01 14:38:40Z yann.regisgianas $ *)


let process raw_ast = 
  try 
    CoreSyntax.import_program CoreSyntax.Identifier.Map.empty raw_ast
  with CoreSyntax.Var.UnboundIdentifier p ->
    Error.error "name analysis" (PIdentifier.position p)
      (Printf.sprintf "The identifier `%s' is unbound."
	 (PIdentifier.as_string p))


