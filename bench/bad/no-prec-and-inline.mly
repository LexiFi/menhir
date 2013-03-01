%start<int> a
%left foo
%%

a: {}

%inline b: %prec foo {} 
