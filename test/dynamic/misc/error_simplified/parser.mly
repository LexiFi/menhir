%token A B C D
%start<unit> main

(* The input sequence specified in main.ml is A D.
   So, the parser should
   shift A,
   detect an error and replace D with an error token,
   reduce bar? -> epsilon,
   reduce head -> A bar?,
   shift error,
   reduce main -> head error,
   stop.

   This test works both with --strategy legacy and --strategy simplified.
 *)

%%

main:
  head C
    { () }
| head error
    { Printf.printf "The parser works as expected.\n%!";
      exit 0 }

head:
  A bar?
    { () }

bar:
  B
    { () }
