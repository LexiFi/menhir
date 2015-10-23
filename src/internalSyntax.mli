type grammar = 
    {
      p_preludes	   : Stretch.t list;
      p_postludes          : Syntax.trailer list;
      p_parameters         : Stretch.t list;
      p_start_symbols      : Positions.t StringMap.t;
      p_types              : (Syntax.parameter * Stretch.ocamltype Positions.located) list;
      p_tokens	           : Syntax.token_properties StringMap.t;
      p_rules	           : Syntax.parameterized_rule StringMap.t;
      p_on_error_reduce    : Syntax.parameter list;
    }
