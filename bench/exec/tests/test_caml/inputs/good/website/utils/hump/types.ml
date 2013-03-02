
type prop_kind =
    Kind
  | License
  | Topic
  | Attribute

let prop_kinds = [Kind ; License ; Topic; Attribute]

type status =
    Pre_alpha
  | Alpha
  | Beta
  | Stable
  | Mature

let status = [ Pre_alpha ; Alpha ; Beta ; Stable ; Mature ]
