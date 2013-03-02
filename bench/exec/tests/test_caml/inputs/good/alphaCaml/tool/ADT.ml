type 'a rhs =
  | Sum of 'a summand list
  | Record of 'a conjunct list
  | Tuple of 'a list

and 'a summand =
    string (* data constructor *) * 'a list

and 'a conjunct =
    string (* field label *) * 'a

let rec map_rhs f = function
  | Sum summands ->
      Sum (map_summands f summands)
  | Record conjuncts ->
      Record (map_conjuncts f conjuncts)
  | Tuple x ->
      Tuple (List.map f x)

and map_summands f summands =
  List.map (map_summand f) summands

and map_conjuncts f conjuncts =
  List.map (map_conjunct f) conjuncts

and map_summand f (data, xs) =
  data, List.map f xs

and map_conjunct f (label, x) =
  label, f x

