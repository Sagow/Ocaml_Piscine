let () =
  let methane = new Alkane.methane
  and dioxygen = new Molecule.dioxygen
  and carbon_dioxide = new Molecule.carbon_dioxide
  and water = new Molecule.water in
  let  balanced_reaction = new Reaction.alkane_combustion [(methane, 1);(dioxygen, 2)] [(carbon_dioxide, 1);(water, 2)] in 
  print_endline balanced_reaction#to_string;
  print_endline ("is balanced : " ^ (Bool.to_string balanced_reaction#is_balanced))