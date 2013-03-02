open Util;;

module WritePlot =
struct
  type t = { plot : Buffer.t ;
    data : Buffer.t;
    mutable png : string;
    mutable number_column : int }
  
  let create () = { plot = (Buffer.create 1024);
    data = (Buffer.create 1024);
    png = "";
    number_column = 0 }
  
  let set_data bufbuf trip format =
    let (pars, gen, dat) = trip in
    bufbuf.number_column <- Array.length gen ;
    Buffer.add_string bufbuf.data "Generator ";
    for i = 0 to Array.length gen - 1 do
      Buffer.add_string bufbuf.data (Printf.sprintf "\"%s\" " gen.(i))
    done;
    Buffer.add_string bufbuf.data (Printf.sprintf "%s" "\n");
    for i = 0 to Array.length pars - 1 do
      Buffer.add_string bufbuf.data (Printf.sprintf "%s " (pars.(i)));
      let length_dat = Array.length dat.(0) in
      for j = 0 to length_dat - 1 do
        Buffer.add_string bufbuf.data (Printf.sprintf (format^^" ") dat.(i).(j))
      done;
      Buffer.add_string bufbuf.data "\n";
    done
  
  let set_int_data bufbuf trip =
    set_data bufbuf trip ("%d": ('a, 'b, 'c) format)
  
  let set_float_data bufbuf trip =
    set_data bufbuf trip ("%f": ('a, 'b, 'c) format)
  
  let set_png bufbuf out =
    bufbuf.png <- out
  
  let write_plot bufbuf title filename =
    Buffer.add_string bufbuf.plot (Printf.sprintf "set title \"%s\"\n" title);
    Buffer.add_string bufbuf.plot "set auto x\n";
    
    if bufbuf.png <> "" then
      (Buffer.add_string bufbuf.plot "set terminal png\n";
        Buffer.add_string bufbuf.plot (Printf.sprintf "set output \"%s.png\"\n"  bufbuf.png));
    
    Buffer.add_string bufbuf.plot "set style data histogram\n";
    Buffer.add_string bufbuf.plot "set style histogram cluster gap 1\n";
    Buffer.add_string bufbuf.plot "set style fill solid border -1\n";
    Buffer.add_string bufbuf.plot "set boxwidth 0.9\n";
    Buffer.add_string bufbuf.plot "set xtic rotate by -45\n";
    Buffer.add_string bufbuf.plot (Printf.sprintf "plot '%s.data' using 2:xtic(1) ti col," filename);
    
    for i = 1 to bufbuf.number_column - 2 do
      Buffer.add_string bufbuf.plot (Printf.sprintf " '' u %d ti col," (i + 2));
    done;
    Buffer.add_string bufbuf.plot (Printf.sprintf " '' u %d ti col" (bufbuf.number_column + 1))
  
  let render_to_file bufbuf title filename = 
    write_plot bufbuf title filename;
    let plot_name = prefix_results (filename ^ ".plot")
    and data_name = prefix_results (filename ^ ".data") in
    let plot_file = open_out plot_name
    and data_file = open_out data_name in
    Printf.printf "Writing the file %s and %s\n" plot_name data_name;
    Printf.printf "if you generate a png : gnuplot %s\n" plot_name;
    Printf.printf "Else, enter in the toplevel of gnuplot\n\t load \"%s\"\n" plot_name;
    Printf.fprintf plot_file "%s" (Buffer.contents bufbuf.plot);
    Printf.fprintf data_file "%s" (Buffer.contents bufbuf.data)
  
end

module Run (A: sig
      val tab_cmo_sizes: (string array) * (string array) * (int array array)
      val tab_ml_sizes: (string array) * (string array) * (int array array)
      val tab_o_sizes: (string array) * (string array) * (int array array)
      val tab_execution_time: (string array) * (string array) * (float array array)
      val tab_generation_time: (string array) * (string array) * (float array array)
      val do_average : bool
    end) =
struct
  
  let make_a_plot set trip title filename =
    let plot = WritePlot.create () in
    set plot trip;
    if Settings.report_with_hist then WritePlot.set_png plot filename;
    WritePlot.render_to_file plot title filename
  
  let make_plot () =
    if A.do_average then
    (
        if Settings.do_ml_size_comparison then
            make_a_plot WritePlot.set_int_data A.tab_ml_sizes ".ml files sizes comparisons" "ml_sizes";
        if Settings.do_cmo_size_comparison then
    make_a_plot WritePlot.set_int_data A.tab_cmo_sizes ".cmo files sizes comparisons" "cmo_sizes";
        if Settings.do_o_size_comparison then
    make_a_plot WritePlot.set_int_data A.tab_o_sizes ".o files sizes comparisons" "o_sizes";
    
    if Settings.bytecode_compilation then
       make_a_plot WritePlot.set_float_data A.tab_execution_time ".byte execution time comparisons" "byte_speeds";
    
    if Settings.native_compilation then
      make_a_plot WritePlot.set_float_data A.tab_execution_time ".opt execution time comparisons" "opt_speeds";
    
      if Settings.do_generation_time_calculation then
    make_a_plot WritePlot.set_float_data A.tab_generation_time "Generation time comparisons" "generation_speeds")
  
end

(*------------------accessors-----------------------*)
(*
let data p =
p ^ ".data"

let plot p =
p ^ ".plot"

let png p =
p ^ ".png"

let ml_sizes = prefix_results "ml_sizes"
let cmo_sizes = prefix_results "cmo_sizes"
let o_sizes = prefix_results "o_sizes"

let byte_speeds = prefix_results "byte_speeds"
let opt_speeds = prefix_results "opt_speeds"

let plot_ml = open_out (plot ml_sizes)
let plot_cmo = open_out (plot cmo_sizes)
let plot_o = open_out (plot o_sizes)

let plot_byte = open_out (plot byte_speeds)
let plot_opt = open_out (plot opt_speeds)

let data_ml = open_out (data ml_sizes)
let data_cmo = open_out (data cmo_sizes)
let data_o = open_out (data o_sizes)

let data_byte = open_out (data byte_speeds)
let data_opt = open_out (data opt_speeds)

let print file fmt str = Printf.fprintf file fmt str;;

let write_plot title file out =
print file "set title \"%s\"\n" title;
print file "%s\n" "set auto x";
(* TODO verifier la hauteru max en fonction du maximum *)
print file "%s\n" "set terminal png";
print file "set output \"%s\"\n" (png out);
print file "%s\n" "set style data histogram";
print file "%s\n" "set style histogram cluster gap 1";
print file "%s\n" "set style fill solid border -1";
print file "%s\n" "set boxwidth 0.9";
print file "%s\n" "set xtic rotate by -45";
print file "plot '%s' using 2:xtic(1) ti col," (data out);
let (_, gen, _) = A.tab_ml_sizes in
let nb_gen = Array.length gen in
for i = 1 to nb_gen - 2 do
print file " '' u %d ti col," (i + 2);
done;
print file " '' u %d ti col" (nb_gen + 1)
;;

let write_data file trip format =
let (pars, gen, dat) = trip in
print file "%s" "Generator ";
for i = 0 to Array.length gen - 1 do
print file "\"%s\" " gen.(i)
done;
print file "%s" "\n";
for i = 0 to Array.length pars - 1 do
print file "%s " (pars.(i));
let length_dat = Array.length dat.(0) in
for j = 0 to length_dat - 1 do
print file (format^^" ") dat.(i).(j)
done;
print file "%s" "\n";
done

let write_data_float file trip =
write_data file trip ("%f": ('a, 'b, 'c, 'd, 'e, 'f) format6)

let write_data_int file trip =
write_data file trip ("%d": ('a, 'b, 'c, 'd, 'e, 'f) format6)

let make_histo title plot data_name data table =
write_plot title plot data_name;
write_data data table

let make_plot () =
write_plot ".ml files sizes comparisons" plot_ml ml_sizes;
write_data_int data_ml A.tab_ml_sizes;
write_plot ".cmo files sizes comparisons" plot_cmo cmo_sizes;
write_data_int data_cmo A.tab_cmo_sizes;
write_plot ".o files sizes comparisons" plot_o o_sizes;
write_data_int data_o A.tab_o_sizes;
write_plot ".byte execution time comparisons" plot_byte byte_speeds;
write_data_float data_byte A.tab_speedsbyte;
write_plot ".opt execution time comparisons" plot_opt opt_speeds;
write_data_float data_opt A.tab_execution_time
;;
*)
