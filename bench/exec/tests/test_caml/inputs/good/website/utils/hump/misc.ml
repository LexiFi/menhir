let _ = Config.dbg "Entering module misc.ml"


let html_of_read_errors l =
  let f = function
      Ocgi.Args.Missing_argument (arg, mes) ->
	mes
    | e ->
	Printexc.to_string e
  in
  String.concat "<br/>" (List.map f l)

let no_blanks = Ocgi.Misc.no_blanks

let opt_of_string = Ocgi.Misc.opt_of_string
let string_of_opt = Ocgi.Misc.string_of_opt

let opt_of_string_opt = function
    None -> None
  | Some s -> opt_of_string s

let map_opt f = function
    None -> None
  | Some v -> Some (f v)

let random_string ?(len=Constant.random_string_length) () =
  let s = Cryptokit.Random.string
	    Cryptokit.Random.secure_rng
	    len
  in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c = Char.chr
	      (
		let n = Char.code s.[i] in
		if n < 48 then
		  (n mod 10 + 48)
		else
		  if n > 57 && n < 65 then
		    (n mod 26) + 65
		  else
		    if n > 90 && n < 97 then
		      (n mod 26) + 97
		    else
		      (n mod 26) + 97
	      )
    in
    Buffer.add_char b c
  done;
  Buffer.contents b

let field s =
  s ^ ": "

let first_sentence ?(limit=Constant.first_sentence_limit) s =
  let rec get_before_dot s =
    try
      let len = String.length s in
      let n = String.index s '.' in
      if n + 1 >= len then
	(* le point est le dernier caractère *)
	(true, s)
      else
	(
	  match s.[n+1] with
            ' ' | '\n' | '\r' | '\t' ->
              (true, String.sub s 0 (n+1))
	  | _ ->
              let (b, s2) = get_before_dot (String.sub s (n + 1) (len - n - 1)) in
              (b, (String.sub s 0 (n+1))^s2)
	)
    with
      Not_found -> (false, s)
  in
  let len = String.length s in
  let s_limited =
    if len > limit then
      String.sub s 0 limit 
    else
      s
  in
  let (found, res) = get_before_dot s_limited in
  if found then
    res
  else
    if len > limit then
      if limit > 3 then
	(String.sub s_limited 0 (limit - 3) ^ "...")
      else
	s_limited
    else
      s
	
let list_diff l1 l2 =
  List.fold_right 
    (fun el acc ->
       if not (List.mem el l2) then
	 el :: acc
       else
	 acc) l1 []

let input_channel ic =
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input ic s 0 len in
      if n = 0 then
        ()
      else
        (
          Buffer.add_substring buf s 0 n;
          iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  Buffer.contents buf

let raw_make_table l =
  let rec f acc row = function
      [] -> List.rev acc
    | cols :: q ->
	let a = 
	  Ocgi.Html.tr ~atts: ["class", Printf.sprintf "row%d" row]
	    (List.map Ocgi.Html.td cols)
	in
	f (a :: acc) ((row + 1) mod 2) q
  in
  Ocgi.Html.table
    ~atts: ["class", "elt_list"]
    (f [] 0 l)

let make_table1 l =
  raw_make_table (List.map (fun s -> [s]) l)

let make_table2 l =
  raw_make_table (List.map (fun (s, s2) -> [s ; s2]) l)

let make_table3 l =
  raw_make_table (List.map (fun (s, s2, s3) -> [s ; s2 ; s3]) l)

let chop_n_char n s =
  let len = String.length s in
  if len <= n +1 or n < 0 then
    s
  else
    Printf.sprintf "%s..." (String.sub s 0 (n+1))

let get_n_first_ele max l =
  let rec iter n l = 
    if n < max then
      match l with
        [] ->
          ([], [])
      | h :: q ->
          let (l1, l2) = iter (n+1) q in
          (h :: l1, l2)
    else
      ([], l)
  in
  iter 0 l

let write_file ~f ~content =
  let oc = open_out f in
  output_string oc content;
  close_out oc

let input_file_as_string nom =
  let chanin = open_in_bin nom in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
