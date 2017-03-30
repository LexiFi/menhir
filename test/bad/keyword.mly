  %token<int> INT
  %start<unit> file
  %%

  file : let=INT { ignore(let) }

