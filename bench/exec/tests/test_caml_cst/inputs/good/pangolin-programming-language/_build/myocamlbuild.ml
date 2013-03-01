open Ocamlbuild_plugin;;
open Command;;

let alphaCaml = A"alphaCaml";;

rule "alphaCaml: mla -> ml & mli"
  ~prods:["%.ml"; "%.mli"]
  ~dep:"%.mla"
  begin fun env _build ->
    Cmd(S[alphaCaml; P(env "%.mla")])
  end;;
