let () =
  let water = new Molecule.water in
  print_endline water#to_string;
  let carbon_dioxide = new Molecule.carbon_dioxide in
  print_endline carbon_dioxide#to_string;
  let trinitrotoluene = new Molecule.trinitrotoluene in
  print_endline trinitrotoluene#to_string;
  let iron3_oxide = new Molecule.iron3_oxide in
  print_endline iron3_oxide#to_string;
  let sodium_oxide = new Molecule.sodium_oxide in
  print_endline sodium_oxide#to_string