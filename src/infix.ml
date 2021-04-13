let ( += ) r i = r := !r + i

let ( *= ) r i = r := !r * i

let ( -= ) r i = r := !r - i

let ( /= ) r i = r := !r / i

let ( ||= ) r i = r := !r || i

let ( &&= ) r i = r := !r && i

let ( @:= ) r f = r := f !r

let ( @@> ) f g x = f (g x)