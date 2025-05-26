let () =
  let alexis = new People.people "Alexis" in
  alexis#talk;
  print_endline alexis#to_string;
  alexis#die
