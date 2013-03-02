 open Util 

module WriteReport =
struct
  type t = Buffer.t
  
  let empty () = Buffer.create 1024
  
  let title buff level str =
    let lign = (String.make (level + 1) '=' ) in
    Buffer.add_string buff (Printf.sprintf "%s %s %s\n\n" lign str lign)
  
  let tild_line =
    "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
  
  (* make_array : t -> string (name of the table)
  -> (string array) * (string array) * (float array array)
  -> format (format for printing cases) -> t *)
  let make_array buff title trip format =
    let (pars, gen, data) = trip in
    let n = Array.length gen - 1 in
    
    (* write the bottom of the table : name, grid, sizes and first line *)
    if title <> "" then
      Buffer.add_string buff (Printf.sprintf "\n.%s" title);
    Buffer.add_string buff (Printf.sprintf "\n%s" "[grid=\"all\"]\n");
    let s = ref "" in
    for i = 0 to n do
      s := !s^"1'"
    done;
    Buffer.add_string buff (Printf.sprintf ".%s%s" !s tild_line);
    Buffer.add_string buff " ,";
    for i = 0 to n - 1 do
      Buffer.add_string buff (Printf.sprintf "%s," gen.(i))
    done;
    Buffer.add_string buff (Printf.sprintf "%s\n" gen.(n));
    Buffer.add_string buff tild_line;
    
    (* write the rest of the table *)
    for i = 0 to Array.length pars - 1 do
      Buffer.add_string buff (Printf.sprintf "%s," pars.(i));
      let length_data = Array.length data.(0) in
      for j = 0 to length_data - 2 do
        Buffer.add_string buff (Printf.sprintf (format^^",") data.(i).(j))
      done;
      Buffer.add_string buff (Printf.sprintf (format^^"\n") data.(i).(length_data - 1))
    done;
    Buffer.add_string buff tild_line;
    Buffer.add_string buff (Printf.sprintf "%s" "\n\n")
  
  let add_int_array buff title trip =
    make_array buff title trip ("%d": ('a, 'b, 'c) format)
  
  let add_float_array buff title trip =
    make_array buff title trip ("%f": ('a, 'b, 'c) format)
  
  let add_paragraph buff str =
    Buffer.add_string buff str
  
  let add_image buff title name =
    Buffer.add_string buff (Printf.sprintf ".%s\n" title );
    Buffer.add_string buff (Printf.sprintf "image::%s[%s]\n\n" (prefix_results name) title)
  
  let render buff =
    Buffer.contents buff
  
  let render_to_file buff filename =
    let name = prefix_results (filename ^ ".txt") in
    let file = open_out name in
    Printf.printf "Writing the file %s\n" name;
    Printf.printf "To generate html and tex file : asciidoc -a toc %s.txt\n" name;
    Printf.fprintf file "%s" (Buffer.contents buff)
  
end

module Run (A: sig
      val tab_cmo_sizes: (string array) * (string array) * (int array array)
      val tab_ml_sizes: (string array) * (string array) * (int array array)
      val tab_o_sizes: (string array) * (string array) * (int array array)
      val average_sizes: ((string array) * (string array) * (int array array)) option
      val tab_execution_time: (string array) * (string array) * (float array array)
      val average_speeds: ((string array) * (string array) * (float array array)) option
      val tab_generation_time: (string array) * (string array) * (float array array)
      val average_speedgeneration : ((string array) * (string array) * (float array array)) option
      val do_average : bool
    end) =
struct
  
  let get_from_option op =
    match op with
    | Some p -> p
    | None -> failwith "get_from : we should never pass by here"
  
  let make_report () =
    let report = WriteReport.empty () in
    (* title of the page *)
    
    if (Settings.do_o_size_comparison
      || Settings.do_cmo_size_comparison
      || Settings.do_ml_size_comparison) then
      WriteReport.title report 0 "Generators comparisons";
    
    (* section 1 : sizes commparisons *)
    if Settings.do_ml_size_comparison then
      (WriteReport.title report 1 "Size comparisons";
        WriteReport.title report 2 " .ml files sizes";
        WriteReport.add_int_array report "" A.tab_ml_sizes;
        if Settings.report_with_hist then
          WriteReport.add_image report "ml files sizes" "ml_sizes.png");
    
    if Settings.do_cmo_size_comparison then
      (WriteReport.title report 2 " .cmo files sizes";
        WriteReport.add_int_array report "" A.tab_cmo_sizes;
        if Settings.report_with_hist then
          WriteReport.add_image report "cmo files sizes" "cmo_sizes.png");
    
    if Settings.do_o_size_comparison then
      (WriteReport.title report 2 " .o files sizes";
        WriteReport.add_int_array report "" A.tab_o_sizes;
        if Settings.report_with_hist then
          WriteReport.add_image report "o files sizes" "o_sizes.png");
    
    (* Section 2 : execution time comparisons *)
    if (Settings.bytecode_compilation || Settings.native_compilation) then
      WriteReport.title report 1 "Execution Time";
    
    if Settings.bytecode_compilation then
      (WriteReport.title report 2 "  .byte execution time";
        WriteReport.add_float_array report "" A.tab_execution_time;
        if Settings.report_with_hist then
          WriteReport.add_image report "ml files sizes" "byte_speeds.png"
      );
    if Settings.native_compilation then
      (WriteReport.title report 2 "  .opt execution time";
        WriteReport.add_float_array report "" A.tab_execution_time;
        if Settings.report_with_hist then
          WriteReport.add_image report "ml files sizes" "opt_speeds.png");
    
    (* Section 3 : Generation time comparisons *)
    if Settings.do_generation_time_calculation then
      (WriteReport.title report 1 "Generation time";
        WriteReport.add_float_array report "" A.tab_generation_time;
        if Settings.report_with_hist then
          WriteReport.add_image report "ml files sizes" "generation_speeds.png");
    
    (* Section 4 : Average's tables *)
    if A.do_average then
      (WriteReport.title report 1 "Average";
        WriteReport.title report 2 "Average Size";
        WriteReport.add_int_array report "" (get_from_option A.average_sizes);
        WriteReport.title report 2 "Average Execution Time";
        WriteReport.add_float_array report "" (get_from_option A.average_speeds);
        WriteReport.title report 2 "Average Generation Time";
        WriteReport.add_float_array report "" (get_from_option A.average_speedgeneration));
    
    (* writing the asciidoc file *)
    WriteReport.render_to_file report "report"
  
end

(*------------------accessors-----------------------*)

(*

let file = open_out "report.txt";;

let print fmt str = Printf.fprintf file fmt str;;

let titre s =
let lign = (String.make (String.length s) '-' ) in
print "%s\n%s\n\n" s lign;;

let soustitre s =
let lign = (String.make (String.length s) '~' ) in
print "%s\n%s\n\n" s lign;;

let ligne () =
print "%s" "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

let bottom_table s n gen =
soustitre s;
print "%s" "[grid=\"all\"]\n";
let s = ref "" in
for i = 0 to n do
s := !s^"1'"
done;
print ".%s~~~~~~~~~~~~~~~~~~~~~~~~~~\n" !s;
print "%s," " ";
for i = 0 to n - 1 do
print "%s," gen.(i)
done;
print "%s\n" gen.(n);
ligne ()

let make_array titre trip format hist hist_name =
let (pars, gen, data) = trip in
bottom_table titre (Array.length gen - 1) gen;
for i = 0 to Array.length pars - 1 do
print "%s," (pars.(i));
let length_data = Array.length data.(0) in
for j = 0 to length_data - 2 do
print (format^^",") data.(i).(j)
done;
print (format^^"\n") data.(i).(length_data - 1)
done;
ligne ();
print "%s" "\n\n";
if hist then
( print ".%s" "histogramme\n";
print "image::%s.png[image]\n\n" (prefix_results hist_name))

let make_int_array titre trip hist hist_name =
make_array titre trip ("%d": ('a, 'b, 'c, 'd, 'e, 'f) format6) hist hist_name

let make_float_array titre trip hist hist_name =
make_array titre trip ("%f": ('a, 'b, 'c, 'd, 'e, 'f) format6) hist hist_name

let comparatif_taille () =
titre "Size comparisons";
make_int_array " .ml files sizes" A.tab_ml_sizes true "ml_sizes";
make_int_array " .cmo files sizes" A.tab_cmo_sizes true "cmo_sizes";
make_int_array " .o files sizes" A.tab_o_sizes true "o_sizes"

let comparatif_vitesse () =
titre "Execution time";
make_float_array "  .byte execution time" A.tab_speedsbyte true "byte_speeds";
make_float_array "  .opt execution time" A.tab_execution_time true "opt_speeds"

let comparatif_vitesse_generation () =
titre "Generation time";
make_float_array "Generation time" A.tab_execution_time false ""

let moyennes () =
titre "Average";
make_int_array "Average Size" A.average_sizes false "";
make_float_array "Average Execution time" A.average_speeds false "";
make_float_array "Average Generation time" A.average_speedgeneration false ""

let head () =
let s = "Generators comparisons" in
let lign = (String.make (String.length s) '=' ) in
print "%s\n%s\n\n" s lign;;

let corps () =
comparatif_taille () ;
comparatif_vitesse ();
comparatif_vitesse_generation ();
moyennes ()

let make_report () =
head ();
corps ()
;;
*)
