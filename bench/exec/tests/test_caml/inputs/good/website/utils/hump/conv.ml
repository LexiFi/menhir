let _ = Config.dbg "Entering module conv.ml"


open Types

let bool_of_string s = s = "1"
let string_of_bool b = if b then "1" else "0"

let bool_of_int = function
    0 -> false
  | _ -> true

let int_of_bool = function
    true -> 1
  | false -> 0

let int_of_prop_kind = function
    Kind -> 0
  | License -> 1
  | Topic -> 2
  | Attribute -> 3

let prop_kind_of_int = function
    0 -> Kind
  | 1 -> License
  | 2 -> Topic
  | 3 -> Attribute
  | n -> failwith (Printf.sprintf "Bad prop_kind code %d" n)

let string_of_prop_kind k = 
  string_of_int (int_of_prop_kind k)

let prop_kind_of_string s =
  prop_kind_of_int (int_of_string s)

let int_of_status = function
    Pre_alpha -> 0
  | Alpha -> 1
  | Beta -> 2
  | Stable -> 3
  | Mature -> 4

let string_of_status s = string_of_int (int_of_status s)

let status_of_int = function
    0 -> Pre_alpha
  | 1 -> Alpha
  | 2 -> Beta 
  | 3 -> Stable
  | 4 -> Mature 
  | n -> failwith (Printf.sprintf "Bad status code %d" n)

let status_of_string s = status_of_int (int_of_string s)
