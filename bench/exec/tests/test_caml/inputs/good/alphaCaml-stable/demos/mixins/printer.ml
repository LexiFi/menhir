open Format
open Strings
open Mm.Raw

let pr = print_string
let sp = print_space
let brk () = print_break 0 0
let nl = force_newline

let print_fields fields =
  StringSet.iter (fun field ->
    pr field;
    sp()
  ) fields

let print_dependencies = function
  | [] ->
      ()
  | ys ->
      pr "[ ";
      List.iter (fun y -> pr y; sp()) ys;
      pr "]";
      sp()

let rec print_atomic_expression = function
  | EVar x ->
      pr x
  | ERecord fields ->
      open_hovbox 0;
      pr "{";
      open_hovbox 2;
      StringMap.iter (fun li ei ->
	nl(); pr "val"; sp();
	pr li;
	sp(); pr "="; sp();
	print_expression ei
      ) fields;
      close_box();
      nl();
      pr "}";
      close_box()
  | ERecordSelection (e, l) ->
      print_atomic_expression e;
      brk();
      pr ".";
      pr l
  | EStructure (input, (output, anonymous)) ->
      open_hovbox 0;
      open_hovbox 2;
      pr "mix";
      (* print the inputs *)
      StringMap.iter (fun l x ->
	nl(); pr "val"; sp();
	pr l;
	(* re-establish pun if possible *)
	if l <> x then begin
	  sp(); pr "as"; sp(); pr x
	end
      ) input;
      (* gather all definitions in a single list
	 and sort it by syntactic key *)
      let defs =
	StringMap.fold (fun l (ys, x, e, k) defs ->
	  (l, ys, x, e, k) :: defs
        ) output
        (List.map (fun (ys, x, e, k) -> ("_", ys, x, e, k)) anonymous)
      in
      let defs =
	List.sort (fun (_, _, _, _, k1) (_, _, _, _, k2) -> Core.kcompare k1 k2) defs
      in
      (* print that list *)
      List.iter (fun (l, ys, x, e, _) ->
	nl(); pr "val"; sp();
	pr l;
	(* re-establish pun if possible *)
	if l <> x then begin
	  sp(); pr "as"; sp(); pr x
	end;
	sp();
	print_dependencies ys;
	pr "="; sp();
	print_expression e
      ) defs;
      close_box();
      nl();
      pr "end";
      close_box()
  | e ->
      pr "(";
      print_expression e;
      pr ")"

and print_unary_expression = function
  | EClose e ->
      pr "close";
      sp();
      print_unary_expression e
  | EDeletion (e, fields) ->
      pr "delete"; sp();
      print_fields fields;
      pr "in"; sp();
      print_unary_expression e
  | EFakeDependency (e, x, y) ->
      pr "fake"; sp(); pr x; sp();
      pr "depends on"; sp(); pr y;
      sp(); pr "in"; sp();
      print_unary_expression e
  | e ->
      print_atomic_expression e

and print_summand_expression = function
  | EComposition (e1, e2) ->
      print_summand_expression e1;
      sp(); pr "+"; sp();
      print_unary_expression e2
  | e ->
      print_unary_expression e

and print_expression = function
  | ELetRec ([], e) ->
      print_expression e
  | ELetRec (binding :: bindings, e) ->
      open_hovbox 0;
      pr "let rec"; sp();
      print_binding binding;
      List.iter (fun binding ->
	nl(); pr "and"; sp();
	print_binding binding
      ) bindings;
      close_box();
      sp(); pr "in"; sp();
      print_expression e
  | e ->
      print_summand_expression e

and print_binding (x, e) =
  pr x; sp(); pr "="; sp();
  print_expression e

