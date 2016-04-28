type grammar =
    { 
      pg_filename          : Syntax.filename;
      pg_declarations      : (Syntax.declaration Positions.located) list;
      pg_rules             : Syntax.parameterized_rule list;
      pg_trailer           : Syntax.trailer option;
    }

    
