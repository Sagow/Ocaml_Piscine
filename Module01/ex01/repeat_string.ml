let rec repeat_string ?(str="x") n =
  if n < 0 then "Error"
  else
    begin
      if n = 0 then ""
      else str ^ (repeat_string ~str:str (n-1))
    end

let () =
  print_endline "n = 5, str = truc";
  print_endline (repeat_string ~str:"truc" 5);
  print_endline "n = 3, str absent";
  print_endline (repeat_string 3);
  print_endline "n = 1, str = truc";
  print_endline (repeat_string ~str:"truc" 1);
  print_endline "n = 0, str = truc";
  print_endline (repeat_string ~str:"truc" 0);
  print_endline "n = -15, str = truc";
  print_endline (repeat_string ~str:"truc" (-15));