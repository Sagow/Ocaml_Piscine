let sum a b =
  a +. b

let () =
  Printf.printf "%f + %f = %f\n" 1.5 2.3 (sum 1.5 2.3);
  Printf.printf "%f + %f = %f\n" (-1.5) 2.3 (sum (-1.5) 2.3);
  Printf.printf "%f + %f = %f\n" 1.5837 2.3938 (sum 11.5837 2.3938)