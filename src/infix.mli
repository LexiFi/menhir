(** [r += n] sets [r] to [!r + n] *)
val ( += ) : int ref -> int -> unit

(** [r *= n] sets [r] to [!r * n] *)
val ( *= ) : int ref -> int -> unit

(** [r -= n] sets [r] to [!r - n] *)
val ( -= ) : int ref -> int -> unit

(** [r /= n] sets [r] to [!r / n] *)
val ( /= ) : int ref -> int -> unit

(** [r ||= n] sets [r] to [!r || n] *)
val ( ||= ) : bool ref -> bool -> unit

(** [r &&= n] sets [r] to [!r && n] *)
val ( &&= ) : bool ref -> bool -> unit

(** [r @:= f] sets [r] to [f !r] *)
val ( @:= ) : 'a ref -> ('a -> 'a) -> unit

(** [f @@> g] is [(fun x -> f (g x))] *)
val ( @@> ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b



