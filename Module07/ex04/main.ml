let () =
  let methane = new Alkane.methane
  and dioxygen = new Molecule.dioxygen
  and carbon_dioxide = new Molecule.carbon_dioxide
  and water = new Molecule.water in
  let  balanced_reaction = new Reaction.alkane_combustion [(methane, 1);(dioxygen, 2)] [(carbon_dioxide, 1);(water, 2)] in 
  print_endline balanced_reaction#to_string;
  print_endline ("is balanced : " ^ (Bool.to_string balanced_reaction#is_balanced));
  let  unbalanced_reaction = new Reaction.alkane_combustion [(methane, 1);(dioxygen, 1)] [(carbon_dioxide, 1);(water, 1)] in 
  print_endline unbalanced_reaction#to_string;
  print_endline ("is balanced : " ^ (Bool.to_string unbalanced_reaction#is_balanced));
  print_endline "balancing...";
  let rebalanced = unbalanced_reaction#balance in
  print_endline rebalanced#to_string;
  print_endline ("is balanced : " ^ (Bool.to_string rebalanced#is_balanced))