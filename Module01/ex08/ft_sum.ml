let rec ft_sum f a b =
  let rec sum_acc f a b acc =
    if a < b then sum_acc f (a+1) b (acc +. (f a))
    else (acc +. (f a))
  in

  if a > b then nan
  else sum_acc f a b 0.0
    


let () =
  print_endline "for (fun i -> float_of_int (i * i)) 1 10";
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_char('\n')