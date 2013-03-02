let build_test filename number = 
  let buf = Buffer.create (number - 1) in
    for i = 0 to number - 2 do
      let r = Random.int (4) in
      match r with
      | 0 -> Buffer.add_char buf 'a'
      | 1 -> Buffer.add_char buf 'b'
      | 2 -> Buffer.add_char buf 'c'
      | 3 -> Buffer.add_char buf 'd'
      | _ -> assert false
    done;
    Buffer.add_char buf 'e';
    let f = open_out filename in
    Buffer.output_buffer f buf;
    close_out f
;;

build_test "trace1" 10000;;
build_test "trace2" 12210;;
build_test "trace3" 12211;;
build_test "speed1" 500000;;
build_test "speed2" 1000000;;
build_test "speed3" 3231230;;

