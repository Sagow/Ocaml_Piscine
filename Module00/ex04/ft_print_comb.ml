let rec check_number number =
  if (Array.length number = 3 && number.(0) < number.(1) && number.(1) < number.(2)) then
    begin
      print_int(number.(0) * 100 + number.(1) * 10 + number.(2));
      match number with
      | [|7; 8; 9|] -> print_string("\n")
      | _ -> print_string(", ")
    end
  else ();
  match number.(2) with
  | 9 -> begin
    match number.(1) with
    | 8 -> if number.(0) < 7 then check_number([|number.(0) + 1; number.(0) + 2; number.(0) + 3|])
    | _ -> check_number([|number.(0); number.(1) + 1; number.(1) + 2|])
  end
  | _ -> check_number([|number.(0); number.(1); number.(2) + 1|])

let ft_print_comb () =
  check_number([|1; 2; 3|])
