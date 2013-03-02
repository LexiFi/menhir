(********************************* VERBOSITY *********************************)
let verbosity = 3
let log_file = "bench.log"


(********************************** DIRECTORIES ******************************)
let results_dir = "results"
let data_dir = "data"
let tests_dir = "tests"

(* parsers subdirectories *)
let inputs_dir = "inputs"
let generated_dir = "generated"
let trace_dir = "traces"

(* This files are in data directory*)
let generators_file = Filename.concat data_dir "generators.xml"
let parsers_file = Filename.concat data_dir "parsers"

(*****************************************************************************)

let do_o_size_comparison = true
let do_cmo_size_comparison = false
let do_ml_size_comparison = true
let do_trace_comparison = true
let do_generation_time_calculation = false
let do_compilation_time_calculation = false

let do_execution_time_calculation = true

let force_generate = false
let force_compile = false
let force_link = false

let native_compilation = true
let bytecode_compilation = false

let forced_generation = force_generate || (do_generation_time_calculation)
let forced_compilation = force_compile || (do_compilation_time_calculation)

let need_notrace_generation = 
  do_o_size_comparison ||
  do_ml_size_comparison ||
  do_cmo_size_comparison ||
  do_execution_time_calculation
;;

let need_trace_generation = 
  do_trace_comparison
;; 

let need_compilation = 
  do_o_size_comparison ||
  do_cmo_size_comparison ||
  do_trace_comparison ||
  do_execution_time_calculation ||
  force_compile
;;

let need_linking = 
  do_trace_comparison ||
  do_execution_time_calculation ||
  force_link
;;

let need_opt = native_compilation
let need_byte = bytecode_compilation

let ocamlc_flags = []

(* FIXME ignored should not be hardcoded *)
let ignored = [".svn"]

let report_with_hist = false 
let do_all_sizes_comparison = do_o_size_comparison && do_ml_size_comparison && do_cmo_size_comparison 
