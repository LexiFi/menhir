module type Flavor = sig

  (* Atoms. *)

  type atom

  (* Abstractions. *)

  type 'a abs

  (* Evidence. *)

  type 'a abstractable

end

