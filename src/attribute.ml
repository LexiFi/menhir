type attribute = {
  key:     key;
  payload: payload;
  origin:  Positions.t;
}

and key =
  string

and payload =
  string

type attributes =
  attribute list

let attribute_has_key key attr =
  key = attr.key

let find_attribute key attrs =
  let attr = List.find (attribute_has_key key) attrs in
  attr.payload

let extract_attribute key attrs =
  MList.extract (attribute_has_key key) attrs

let transform_attribute key f attrs =
  let oattr, remainder = extract_attribute key attrs in
  match oattr with
  | None ->
      attrs
  | Some attr ->
      { attr with payload = f attr.payload } :: remainder
