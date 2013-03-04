(* $Id: concreteSyntax.mli,v 1.3 2005/12/01 16:20:06 regisgia Exp $ *)
type grammar =
    { 
      pg_filename	   : Syntax.filename;
      pg_declarations	   : (Syntax.declaration Positions.located) list;
      pg_rules             : Syntax.parameterized_rule list;
      pg_trailer	   : Syntax.trailer option;
    }

    
