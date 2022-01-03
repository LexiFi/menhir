%token FOO BAR EOL

%start<Syntax.ast> main

%%

(* Because [Foo] is not qualified and we have not opened [Syntax], the
   semantic action [Foo] is problematic. When type inference is enabled,
   type-directed disambiguation can kick in (this is somewhat of a miracle)
   and find out that [Foo] really means [Syntax.Foo]. With the old code
   back-end and with the table back-end, this works, because semantic actions
   are decorated with their inferred type. In the new code back-end, in
   version 20211230, this does not work, because no type annotation is
   emitted. *)

main:
| FOO EOL { Foo }
| BAR EOL { Syntax.Bar }
