let () =
  let doc = new Doctor.doctor "11th" in
  doc#talk;
  print_endline doc#to_string;
  doc#travel_in_time 1400 2947;
  print_endline doc#to_string;
  doc#use_sonic_screwdriver;
  doc#lose_hp 50;
  print_endline doc#to_string;
  (* doc#regenerate; *)
  doc#lose_hp (-1);
  print_endline doc#to_string;
