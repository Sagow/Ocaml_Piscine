let rec fibonacci n =
  let rec fibo_acc n i acc =
    if n < 0 then -1
    else if n = 0 then acc
    else fibo_acc (n-1) acc (i+acc)
  in
  fibo_acc n 0 1

let () =
  print_endline "for 0";
  print_int (fibonacci 0);
  print_char('\n');
  print_endline "for 1;";
  print_int (fibonacci 1;);
  print_char('\n');
  print_endline "for 5;";
  print_int (fibonacci 5;);
  print_char('\n');
  print_endline "for 6";
  print_int (fibonacci 6);
  print_char('\n');
  print_endline "for -42";
  print_int (fibonacci (-42));
  print_char('\n')