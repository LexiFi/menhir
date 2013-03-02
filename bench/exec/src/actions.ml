open GeneratorsDB
open GeneratedParser

(*****************************************************************************)
(******************************** Parsers Generation *************************)
(*****************************************************************************)
let generate_all trace force =
  if trace then
    (
      GeneratorsList.iter
        (fun gen -> ParsersDB.iter (
                  fun parser ->
                      GeneratedParser.Generated.make gen parser true
                )
        );
      GeneratedParser.Generated.iter_trace (Commands.generate force)
    )
  else
    (
      GeneratorsList.iter
        (fun gen -> ParsersDB.iter (
                  fun parser ->
                      GeneratedParser.Generated.make gen parser false
                )
        );
      GeneratedParser.Generated.iter_notrace (Commands.generate force)
    );
;;

(*****************************************************************************)
(****************************** PARSERS COMPILATION **************************)
(*****************************************************************************)
let compile_all opt force =
  GeneratedParser.Generated.iter_all (Commands.compile opt force)

let compile_trace opt force =
  GeneratedParser.Generated.iter_trace (Commands.compile opt force)

let compile_notrace opt force =
  GeneratedParser.Generated.iter_notrace (Commands.compile opt force)

let compile_additionals opt force = 
  ParsersDB.iter (Commands.copy_additionals);
  ParsersDB.iter (Commands.compile_additionals opt force)

let compile_lexers opt force = 
  GeneratedParser.Generated.iter_all (Commands.generate_lexer opt force);
  GeneratedParser.Generated.iter_all (Commands.compile_lexer opt force)

let link_trace opt force =
  GeneratedParser.Generated.iter_trace (Commands.link opt force)

let link_notrace opt force =
  GeneratedParser.Generated.iter_notrace (Commands.link opt force)

let link_all opt force =
  GeneratedParser.Generated.iter_all (Commands.link opt force)

(*****************************************************************************)
(******************************* SPEED CALCULATION ***************************)
(*****************************************************************************)
let run_speeds_all opt =
  GeneratedParser.Generated.iter_notrace
    (fun generated ->
          let inputs = Inputs.get_good_inputs_list generated in
          List.iter (fun input -> Commands.run_speed generated opt input)
            inputs
    )
;;
let run_traces_all opt =
  GeneratedParser.Generated.iter_trace
    (fun generated ->
          let inputs = Inputs.get_all_inputs_list generated in
          List.iter (fun input -> Commands.run_trace generated opt input)
            inputs
    )
;;


let compare_traces_all opt = 
  ParsersDB.iter (
    fun parser -> 
      GeneratorsDB.GeneratorsList.iter_group (
        fun generatorsgroup ->
          let generator1, generator2 = generatorsgroup in
          let generated1 = Generated.get_generated_parsers_trace generator1 parser
          and generated2 = Generated.get_generated_parsers_trace generator2 parser in
          List.iter (
            fun input -> Commands.diff_traces generated1 generated2 opt input
            
          ) 
          (* FIXME change inputs mapping from generated to parser *)
          (Inputs.get_all_inputs_list generated1)
      )
  )
          
;;

(*****************************************************************************)
(********************************* Size calculations *************************)
(*****************************************************************************)
(* FIXME error handling *)
let calc_ml_size p =
  let fstat = Unix.stat (GeneratedParser.Generated.get_absolute_ml_filename p) in
  GeneratedParser.Generated.set_ml_size p fstat.Unix.st_size
;;

let calc_cmo_size p =
  let fstat = Unix.stat (GeneratedParser.Generated.get_absolute_cmo_filename p) in
  GeneratedParser.Generated.set_cmo_size p fstat.Unix.st_size
;;

let calc_o_size p =
  let fstat = Unix.stat (GeneratedParser.Generated.get_absolute_o_filename p) in
  GeneratedParser.Generated.set_o_size p fstat.Unix.st_size
;;

let calc_all_sizes p =
  calc_o_size p;
  calc_ml_size p;
  calc_cmo_size p
;;

(*
let compile_with_lexer () =
ParsersDB.iter_generated (fun p -> if (GeneratedParser.is_onlysizes p) then () else GeneratedParser.compile_with_lexer false p );
ParsersDB.iter_generated (fun p -> if (GeneratedParser.is_onlysizes p) then () else GeneratedParser.compile_with_lexer true p )
;;
*)

(* FIXME TRES SALE *)
(*
module GenerateReport (T: sig end) =
struct
  
  let generate_tab iter f =
    let lst = ref [] in
    iter (fun x -> lst := (f x):: (!lst));
    Array.of_list !lst
  
 
  
  let array_to_matrix tab x y empty_tab =
    let result = empty_tab in
    for i = 0 to x - 1 do
      for j = 0 to y - 1 do
        result.(i).(j) <- (tab.(j + i * (y)));
      done;
    done;
    result
  
  let array_to_float_matrix tab x y =
    array_to_matrix tab x y (Array.make_matrix x y 0.0)
  
  let array_to_int_matrix tab x y =
    array_to_matrix tab x y (Array.make_matrix x y 0)
  
  let generators = GeneratorsList.generators_name ()
  let parsers = generate_tab ParsersDB.iter ParsersDB.MlyParser.get_filename
  
  let length_gen = Array.length generators
  let length_parsers = Array.length parsers
  
  (******************generation of sizes tables*********************)
  
  let tab_ml_sizes =
    let result = array_to_int_matrix (generate_tab GeneratedParser.Generated.iter_notrace GeneratedParser.Generated.get_ml_size ) length_parsers length_gen in
    (parsers, generators, result)
  
  let tab_cmo_sizes =
    let result = array_to_int_matrix( generate_tab GeneratedParser.Generated.iter_notrace GeneratedParser.Generated.get_cmo_size) length_parsers length_gen in
    (parsers, generators, result)
  
  let tab_o_sizes =
    let result = array_to_int_matrix( generate_tab GeneratedParser.Generated.iter_notrace GeneratedParser.Generated.get_o_size) length_parsers length_gen in
    (parsers, generators, result)
  
  (**************************generation of speeds tables******************************)
  
   let average_inputs_list list =
    let rec aux lst acc nb =
      match lst with
      | [] -> acc,nb
      | x:: y -> match (Inputs.Input.get_result x) with 
        | Inputs.Input.OK p -> aux y (p +. acc) nb
        | _  -> aux y acc (nb-1)
    in
    let (a,b) = aux list 0.0 (List.length list) in
    a
    (*if b = 0 then 0. else a/.(float_of_int b)*)
    
  
  let generate_execution_time iter f =
    let lst = ref [] in
    iter (fun x -> lst := (average_inputs_list (f x)):: (!lst));
    Array.of_list !lst
  
  
  let tab_execution_time =
    let result = array_to_float_matrix (generate_execution_time GeneratedParser.Generated.iter_notrace Inputs.get_good_inputs_list) length_parsers length_gen in
    (parsers, generators, result)
  
  (*let tab_speedsopt =
    let result = array_to_float_matrix (generate_speed GeneratedParser.Generated.iter_notrace GeneratedParser.Generated.get_speedsopt) length_parsers length_gen in
    (parsers, generators, result)
  *)
  (*************************generation of speed generation***************************)
  
  let tab_generation_time =
    let result = array_to_float_matrix (generate_tab GeneratedParser.Generated.iter_notrace GeneratedParser.Generated.get_generation_time) length_parsers length_gen in
    (parsers, generators, result)
  
  (**************************generation of average's tables***************************)
  let do_average = ref true;;
  
  if length_parsers = 1 then
    do_average := false
  
  let do_average = !do_average;;
  
  let plus_int a b =
    a + b
  
  let plus_float a b =
    a +. b
  
  let div_int a b =
    a / b
  
  let div_float a b =
    a /. (float_of_int b)
  
  let calc_average matrix plus empty_tab =
    let result = empty_tab in
    for i = 0 to Array.length matrix - 1 do
      for j = 0 to Array.length matrix.(0) - 1 do
        result.(j) <- plus result.(j) matrix.(i).(j)
      done;
    done;
    result
  
  let calc_average_int matrix =
    calc_average matrix plus_int (Array.make length_gen 0)
  
  let calc_average_float matrix =
    calc_average matrix plus_float (Array.make length_gen 0.0)
  
  let average data names calc_fun div =
    let length = Array.length data in
    assert (length = Array.length names);
    let result = Array.make length [||] in
    for i = 0 to length - 1 do
      let (_, _, tmp) = data.(i) in
      result.(i) <- calc_fun tmp
    done;
    let nb_pars = length_parsers in
    let res = Array.map (fun y -> Array.map (fun x -> div x nb_pars) y ) result in
    (names, generators, res)
  
  let set_tab_of_sizes () =
    let s = ref [||] 
    and t = ref [||] in 
    if Settings.do_ml_size_comparison then
      (s := Array.append [|tab_ml_sizes|] (!s);
      t := Array.append [|"ml sizes"|] (!t));
    if Settings.do_cmo_size_comparison then
      (s := Array.append [|tab_cmo_sizes|] (!s);
      t := Array.append [|"cmo sizes"|] (!t));
    if Settings.do_o_size_comparison then
      (s := Array.append [|tab_o_sizes|] (!s);
      t := Array.append [|"o sizes"|] (!t));
      (!s,!t)
      
  
  let average_sizes =
    if do_average then
      let (s,t) = set_tab_of_sizes () in
      Some (average s t calc_average_int div_int)
    else None
  
  let average_speeds =
    if do_average then
      Some (average [| (*tab_speedsbyte;*) tab_execution_time |] [|(*"speed byte";*) "speed opt" |] calc_average_float div_float)
    else None
  
  let average_speedgeneration =
    if do_average then
      Some (average [| tab_generation_time |] [|"speed generation" |] calc_average_float div_float)
    else None
  
  (*
  ParsersDB.iter_generated
  (fun x -> Printf.printf "nom : %s et taille_ml : %d\n"
  (GeneratedParser.get_absolute_ml_filename x)
  (GeneratedParser.get_ml_size x)
  )
  *)

end
*)
