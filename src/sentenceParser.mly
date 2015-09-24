/* This is two parsers in one. */

/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is set. The entry point is [optional_sentence]. */

/* It is used also to read a [.messages] file. The entry point is [entry]. */

/* ------------------------------------------------------------------------ */
/* Tokens. */

%token COLON EOF EOL
%token<Grammar.Terminal.t> TERMINAL
%token<Grammar.Nonterminal.t> NONTERMINAL

/* ------------------------------------------------------------------------ */
/* Types. */

%{

  open Grammar
  type terminals = Terminal.t list
  type sentence = Nonterminal.t option * terminals
  type located_sentence = Positions.positions * sentence

%}

%type <terminals> terminals
%type <sentence> sentence
%type <located_sentence> located_sentence

/* %start <sentence option> optional_sentence */
%type
  <(Grammar.Nonterminal.t option * Grammar.Terminal.t list) option>
optional_sentence
%start optional_sentence

/* %start<located_sentence list> entry */
%type
  <(Positions.positions * (Grammar.Nonterminal.t option * Grammar.Terminal.t list)) list>
entry
%start entry

%%

/* ------------------------------------------------------------------------ */

/* An entry is a non-empty list of located sentences. */
entry: located_sentence located_sentences EOF
  { $1 :: $2 }

/* A list of located sentences. */
located_sentences: { [] } | located_sentence located_sentences { $1 :: $2 }

/* A located sentence. */
located_sentence: sentence
  { let pos = Positions.two
      (Parsing.symbol_start_pos())
      (Parsing.symbol_end_pos()) in
    pos, $1 }

/* An optional sentence. */
optional_sentence:
| EOF
    { None } 
| sentence
    { Some $1 }

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. It is terminated by a newline. */
sentence:
| NONTERMINAL COLON terminals EOL
    { Some $1, $3 }
| terminals EOL
    { None, $1 }

/* A list of terminal symbols. */
terminals:
| 
    { [] } 
| TERMINAL terminals
    { $1 :: $2 }

