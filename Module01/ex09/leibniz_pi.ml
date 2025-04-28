let leibniz_pi delta =
  let abs_float diff =
    if diff < 0. then (diff *. (-1.))
    else diff
  in
  let rec power a b res =
    if a > 0 then power (a - 1) b (res *. b)
    else res
  in
  let rec leib_rec pi i delta =
    let next_pi = pi +. 4. *. (power i (-1.) 1.) /. (float_of_int (i * 2 + 1)) in
      if abs_float(next_pi -. (4. *. (atan 1.))) > delta then leib_rec next_pi (i+1) delta
      else i
  in
  leib_rec 0. 0 (abs_float delta)

let () =
  print_endline "for delta = 0.5";
  print_int (leibniz_pi 0.5);
  print_newline ();
  print_endline "for delta = 0.05";
  print_int (leibniz_pi 0.05);
  print_newline ();
  print_endline "for delta = 0.005";
  print_int (leibniz_pi 0.005);
  print_newline ();
  print_endline "for delta = -2";
  print_int (leibniz_pi (-2.));
  print_newline ()