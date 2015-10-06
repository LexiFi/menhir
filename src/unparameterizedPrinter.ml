open Positions
open Syntax
open Stretch
open UnparameterizedSyntax
open Settings

let print_preludes f g =
  List.iter (fun prelude ->
    Printf.fprintf f "%%{%s%%}\n" prelude.stretch_raw_content
  ) g.preludes

let print_start_symbols b g = 
  StringSet.iter (fun symbol ->
    Printf.fprintf b "%%start %s\n" (Misc.normalize symbol)
  ) g.start_symbols
    
let rec insert_in_partitions item m = function
  | [] -> 
      [ (m, [ item ]) ]
	
  | (m', items) :: partitions when Mark.same m m' -> 
      (m', item :: items) :: partitions
	
  | t :: partitions ->
      t :: (insert_in_partitions item m partitions)
     
let insert (undefined, partitions) = function
  | (item, UndefinedPrecedence) ->
      ((item, 0) :: undefined, partitions)
	
  | (item, PrecedenceLevel (m, v, _, _)) ->
      (undefined, insert_in_partitions (item, v) m partitions)

let print_ocamltype ocamltype =
  Printf.sprintf " <%s>" (
    match ocamltype with
    | Declared stretch ->
	stretch.stretch_raw_content
    | Inferred t ->
	t
    )

let print_assoc = function
  | LeftAssoc ->
      Printf.sprintf "%%left"
  | RightAssoc ->
      Printf.sprintf "%%right"
  | NonAssoc ->
      Printf.sprintf "%%nonassoc"
  | UndefinedAssoc ->
      ""

let print_tokens mode b g = 
  (* Sort tokens wrt precedence. *)
  let undefined, partition_tokens = 
    StringMap.fold (fun token prop acu ->
      insert acu (token, prop.tk_priority)
    ) g.tokens ([], [])
  in
  let ordered_tokens =
    List.fold_left (fun acu (_, ms) -> 
      acu @ List.sort (fun (_, v) (_, v') -> compare v v') ms
    ) undefined partition_tokens
  in
  List.iter (fun (token, _) ->
    let prop = StringMap.find token g.tokens in
    if prop.tk_is_declared then
      Printf.fprintf b "%%token%s %s\n"
	begin match mode with
	| PrintNormal
	| PrintUnitActions ->
	    Misc.o2s prop.tk_ocamltype print_ocamltype
	| PrintUnitActionsUnitTokens ->
	    "" (* omitted ocamltype after %token means <unit> *)
	end
	token
  ) ordered_tokens;

  ignore (List.fold_left 
	    (fun last_prop (token, v) -> 
	       let prop = StringMap.find token g.tokens in 
		 match last_prop with

		   | None ->
		       if prop.tk_associativity = UndefinedAssoc then
			 None
		       else (
			 Printf.fprintf b "%s %s "
			   (print_assoc prop.tk_associativity) token;
			 Some v)
			 
		   | Some v' when v <> v' -> 
		       if prop.tk_associativity = UndefinedAssoc then
			 None
		       else (
			 Printf.fprintf b "\n%s %s "
			   (print_assoc prop.tk_associativity) token;
			 Some v)
			 
		   | Some _ -> 
		       Printf.fprintf b "%s " token;
		       last_prop
			 
	    ) None ordered_tokens);
  Printf.fprintf b "\n"

let print_types mode b g = 
  StringMap.iter (fun symbol ty ->
    Printf.fprintf b "%%type%s %s\n" 
      begin match mode with
      | PrintNormal ->
	  print_ocamltype ty
      | PrintUnitActions
      | PrintUnitActionsUnitTokens ->
	  " <unit>"
      end
      (Misc.normalize symbol)
  ) g.types

let binding mode id =
  match mode with
  | PrintNormal ->
      id ^ " = "
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ""

let string_of_producer mode (symbol, ido) =
  binding mode ido ^ (Misc.normalize symbol)

let print_branch mode f branch = 
  Printf.fprintf f "%s%s\n    {"
    (String.concat " " (List.map (string_of_producer mode) branch.producers))
    (Misc.o2s branch.branch_prec_annotation (fun x -> " %prec "^x.value));
  begin match mode with
  | PrintNormal ->
      Action.print f branch.action  
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      Printf.fprintf f "()"
  end;
  Printf.fprintf f "}\n"

let print_trailers b g =
  List.iter (fun stretch -> Printf.fprintf b "%s\n" stretch.stretch_raw_content) g.postludes

(* Because the resolution of reduce/reduce conflicts is implicitly dictated by
   the order in which productions appear in the grammar, the printer should be
   careful to preserve this order. *)
let branches_order r r' = 
  let branch_order b b' = 
    match b.branch_production_level, b'.branch_production_level with
      | ProductionLevel (m, l), ProductionLevel (m', l') ->
	  if Mark.same m m' then
	    if l < l' then
	      -1
	    else if l > l' then
	      1
	    else 
	      0
	  else 0
  in
  let rec lexical_order bs bs' = 
    match bs, bs' with
      | [], [] ->
	  0
      | [], _ ->
	  -1
      | _, [] ->
	  1
      | b :: bs, b' :: bs' ->
	  match branch_order b b' with
	    | 0 -> 
		lexical_order bs bs'
	    | x -> 
		x
  in
    lexical_order r.branches r'.branches

let print_rules mode b g = 
  let rules_as_list =
    StringMap.fold (fun nt r acu -> (nt, r) :: acu) g.rules []
  in
  let ordered_rules =
    List.sort (fun (_nt, r) (_nt', r') -> branches_order r r') rules_as_list
  in
  List.iter (fun (nt, r) ->
    Printf.fprintf b "\n%s:\n" (Misc.normalize nt);
    List.iter (fun br -> 
      Printf.fprintf b "| ";
      print_branch mode b br
    ) r.branches
  ) ordered_rules

let print mode f g =
  begin match mode with
  | PrintNormal ->
      print_preludes f g
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ()
  end;
  print_start_symbols f g;
  print_tokens mode f g;
  print_types mode f g;
  Printf.fprintf f "%%%%\n";
  print_rules mode f g;
  Printf.fprintf f "\n%%%%\n";
  begin match mode with
  | PrintNormal ->
      print_trailers f g
  | PrintUnitActions
  | PrintUnitActionsUnitTokens ->
      ()
  end

