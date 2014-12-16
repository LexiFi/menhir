/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is enabled. */

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. */

%token COLON EOF EOL
%token<Grammar.Terminal.t> TERMINAL
%token<Grammar.Nonterminal.t> NONTERMINAL

%type <(Grammar.Nonterminal.t option * Grammar.Terminal.t list) option> sentence
%start sentence

%%

sentence:
| EOF
    { None } 
| NONTERMINAL COLON terminals EOL
    { Some (Some $1, $3) }
| terminals EOL
    { Some (None, $1) }

terminals:
| 
    { [] } 
| TERMINAL terminals
    { $1 :: $2 }

