/* This is two parsers in one. */

/* This parser is used to read the sentences provided on the standard input
   channel when [--interpret] is set. The entry point is [optional_sentence]. */

/* It is used also to read a [.messages] file. The entry point is [entry]. */

/* ------------------------------------------------------------------------ */
/* Tokens. */

%token COLON EOF EOL
%token<Grammar.Terminal.t> TERMINAL
%token<Grammar.Nonterminal.t> NONTERMINAL
%token<string> COMMENT
  /* only manually-written comments, beginning with a single # */

/* ------------------------------------------------------------------------ */
/* Types. */

%{

  open SentenceParserAux

%}

%type <terminals> terminals
%type <sentence> sentence
%type <located_sentence> located_sentence

%type <SentenceParserAux.sentence option> optional_sentence
%start optional_sentence

%type<SentenceParserAux.located_sentence SentenceParserAux.or_comment list> entry
%start entry

%%

/* ------------------------------------------------------------------------ */

/* An entry is a list of located sentences or comments. */
entry: located_sentences_or_comments EOF
  { $1 }

/* A list of located sentences or comments. */
located_sentences_or_comments:
  { [] }
| located_sentence located_sentences_or_comments { Thing   $1 :: $2 }
| COMMENT          located_sentences_or_comments { Comment $1 :: $2 }

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

