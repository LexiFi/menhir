type 'a result =
  | Unknown
  | Success of ('a * int)
  | Failure

exception Next

let parse s =
  let length = String.length s in

  let dvAdditive = Array.make (length + 1) Unknown
  and dvMultitive = Array.make (length + 1) Unknown
  and dvPrimary = Array.make (length + 1) Unknown
  and dvDecimal = Array.make (length + 1) Unknown in

  let rec pAdditive i =
    match dvAdditive.(i) with
    | Success result ->
	result
    | Failure ->
	raise Next
    | Unknown ->
	try
	  let result = 
	    try
	      let (vleft, i') = pMultitive i in
	      if (i' < length) && (s.[i'] = '+') then
		let (vright, i'') = pAdditive (i'+1) in
		(vleft + vright, i'')
	      else
		raise Next
	    with Next ->
	      pMultitive i
	  in
	  dvAdditive.(i) <- Success result;
	  result
	with Next ->
	  dvAdditive.(i) <- Failure;
	  raise Next

  and pMultitive i =
    match dvMultitive.(i) with
    | Success result ->
	result
    | Failure ->
	raise Next
    | Unknown ->
	try
	  let result =    
	    try
	      let (vleft, i') = pPrimary i in
	      if (i' < length) && (s.[i'] = '*') then
		let (vright, i'') = pMultitive (i'+1) in
		(vleft * vright, i'')
	      else
		raise Next
	    with Next ->
	      pPrimary i
	  in
	  dvMultitive.(i) <- Success result;
	  result
	with Next ->
	  dvMultitive.(i) <- Failure;
	  raise Next

  and pPrimary i =
    match dvPrimary.(i) with
    | Success result ->
	result
    | Failure ->
	raise Next
    | Unknown ->
	try
	  let result =    
	    try
	      if (i < length) && (s.[i] = '(') then
		let (v, i') = pAdditive (i+1) in
		if (i' < length) && (s.[i'] = ')') then
		  (v, i'+1)
		else
		  raise Next
	      else
		raise Next
	    with Next ->
	      pDecimal i
	  in
	  dvPrimary.(i) <- Success result;
	  result
	with Next ->
	  dvPrimary.(i) <- Failure;
	  raise Next

  and pDecimal i =
    match dvDecimal.(i) with
    | Success result ->
	result
    | Failure ->
	raise Next
    | Unknown ->
	try
	  let result =    
	    if (i < length) then
	      match s.[i] with
	      | '0' ->
		  (0, i+1)
	      | '1' ->
		  (1, i+1)
	      | '2' ->
		  (2, i+1)
	      | '3' ->
		  (3, i+1)
	      | '4' ->
		  (4, i+1)
	      | '5' ->
		  (5, i+1)
	      | '6' ->
		  (6, i+1)
	      | '7' ->
		  (7, i+1)
	      | '8' ->
		  (8, i+1)
	      | '9' ->
		  (9, i+1)
	      | _ ->
		  raise Next
	    else
	      raise Next
	  in
	  dvDecimal.(i) <- Success result;
	  result
	with Next ->
	  dvDecimal.(i) <- Failure;
	  raise Next

  in
  try
    let (v, i) = pAdditive 0 in
    if i < length then
      Printf.printf "Warning: trailing garbage ignored at character %d.\n" i;
    Printf.printf "The value is %d.\n" v
  with Next ->
    Printf.printf "Grammatical error.\n"

let _ =
  parse "2+(3*4*2+1*2+3)+7*(3+2)"

