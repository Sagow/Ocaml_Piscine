let rec ackermann m n =
  if (m < 0 || n < 0) then -1
  else
    begin
      if m = 0 then n + 1
      else
        begin
          if n = 0 then ackermann (m - 1) 1
          else ackermann (m - 1) (ackermann m (n - 1))
        end
    end

  let () =
  print_endline "for -1 7";
  print_int (ackermann (-1) 7);
  print_char('\n');
  print_endline "for 0 0";
  print_int (ackermann 0 0);
  print_char('\n');
  print_endline "for 2 3";
  print_int (ackermann 2 3);
  print_char('\n');
  print_endline "4 1";
  print_int (ackermann 4 1)