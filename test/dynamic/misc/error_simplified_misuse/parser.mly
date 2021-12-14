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
   then detect that the semantic action has not aborted the parser,
   as it should.

   This test works with --strategy simplified. *)

%%

main:
  head C
    { () }
| head error
    { Printf.printf "The production main -> head error is reduced, as expected.\n%!" }
      (* This is where we should raise an exception or call [exit],
         but do not do so. *)

head:
  A bar?
    { () }

bar:
  B
    { () }
