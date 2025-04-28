let rec converges f x n =
  if n < 0 then false
  else if n = 0 then (f x) = x
  else (converges f (f x) (n-1))

let () =
  print_endline "for (fun x -> x * x) 2 4";
  print_endline (string_of_bool ( (converges (fun x -> x * x) 2 4)));
  print_endline "for (( * ) 2) 2 5";
  print_endline (string_of_bool ( (converges (( * ) 2) 2 5)));
  print_endline "for (fun x -> x * 2) 0 4";
  print_endline (string_of_bool ( (converges (fun x -> x * 2) 0 4)));
  print_endline "for (fun x -> x * 2) 2 0";
  print_endline (string_of_bool ( (converges (fun x -> x * 2) 2 0)));
  print_endline "for (fun x -> x / 2) 2 3";
  print_endline (string_of_bool ( (converges (fun x -> x / 2) 2 3)));
  print_endline "for (fun x -> x) 2 3";
  print_endline (string_of_bool ( (converges (fun x -> x) 2 3)));