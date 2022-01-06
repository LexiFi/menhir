%token A EOF
%start<int> main
%type<unit> a
%%
let a := A; <A>

let main := a; EOF; <Main>
