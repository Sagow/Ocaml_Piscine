let rec repeat_x n =
  if n < 0 then "Error"
  else
    begin
      if n = 0 then ""
      else "x" ^ repeat_x (n-1)
    end

let () =
  print_endline "n = 15";
  print_endline (repeat_x 15);
  print_endline "n = 3";
  print_endline (repeat_x 3);
  print_endline "n = 1";
  print_endline (repeat_x 1);
  print_endline "n = 0";
  print_endline (repeat_x 0);
  print_endline "n = -15";
  print_endline (repeat_x (-15));