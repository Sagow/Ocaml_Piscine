let () =
  let hydrogen = new Atom.hydrogen
  and carbon = new Atom.carbon
  and oxygen = new Atom.oxygen
  and nitrogen = new Atom.nitrogen
  and sodium = new Atom.sodium
  and iron = new Atom.iron in
  print_endline hydrogen#to_string;
  print_endline carbon#to_string;
  print_endline oxygen#to_string;
  print_endline nitrogen#to_string;
  print_endline sodium#to_string;
  print_endline iron#to_string;
  print_endline ("hydrogen = carbon ? " ^ Bool.to_string(hydrogen#equals carbon));
  let other_hydrogen = new Atom.hydrogen in
  print_endline ("hydrogen = other_hydrogen ? " ^ Bool.to_string(hydrogen#equals other_hydrogen))
