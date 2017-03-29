/* This grammar is accepted by ocamlyacc, but rejected by Menhir,
   as there are two definitions of the symbol [a], and we view
   this as a potential mistake. We allow splitting the definition
   of %public symbols only. */
%token A B C
%start a
%type<unit> a
%%
a: A B C {}
a: B     {}
