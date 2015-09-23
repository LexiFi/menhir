/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is enabled. */

/* It is used also to read a [.messages] file. This is two parsers in one. */

/* ------------------------------------------------------------------------ */
/* Tokens. */

%token COLON EOF EOL
%token<Grammar.Terminal.t> TERMINAL
%token<Grammar.Nonterminal.t> NONTERMINAL
/* A block of text, preceded with '=' and terminated with a blank line. */
%token<string> BLOCK

/* ------------------------------------------------------------------------ */
/* Types. */

%{

  open Grammar
  type terminals = Terminal.t list
  type sentence = Nonterminal.t option * terminals
  type located_sentence = Positions.positions * sentence
  type message = string
  type entry = located_sentence list * message
  type file = entry list

%}

%type <terminals> terminals
%type <sentence> sentence
%type <located_sentence> located_sentence
%type <entry> entry
%type <file> entries

/* %start <sentence option> optional_sentence */
%type <(Grammar.Nonterminal.t option * Grammar.Terminal.t list) option> optional_sentence
%start optional_sentence

/* %start <file> file */
%type <((Positions.positions * (Grammar.Nonterminal.t option * Grammar.Terminal.t list)) list * string) list> file
%start file

/* %start<located_sentence list> entry1 */
%type <(Positions.positions * (Grammar.Nonterminal.t option * Grammar.Terminal.t list)) list> entry1
%start entry1

%%

/* ------------------------------------------------------------------------ */

/* A file is a list of entries, terminated with an end-of-file. */
file: entries EOF { $1 }

/* A list of entries. */
entries: { [] } | entry entries { $1 :: $2 }

/* An entry is a non-empty list of located sentences, followed with a block of text. */
entry: entry1 BLOCK
  { $1, $2 }
entry1: located_sentence located_sentences
  { $1 :: $2 }
| EOF
  { [] } /* a bit of a hack */

/* A list of located sentences. */
located_sentences: { [] } | located_sentence located_sentences { $1 :: $2 }

/* A located sentence. (Must be non-empty, because we use blank lines as delimiters.) */
located_sentence: nonempty_sentence
  { let pos = Positions.two (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos()) in
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

/* A sentence is a pair of an optional non-terminal start symbol and a list
   of terminal symbols. It is terminated by a newline. */
nonempty_sentence:
| NONTERMINAL COLON TERMINAL terminals EOL
    { Some $1, $3 :: $4 }
| TERMINAL terminals EOL
    { None, $1 :: $2 }

/* A list of terminal symbols. */
terminals:
| 
    { [] } 
| TERMINAL terminals
    { $1 :: $2 }

