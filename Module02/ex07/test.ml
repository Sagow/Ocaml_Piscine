

let () =
  let h = Ribosome.generate_helix 24 in
  let s = Ribosome.helix_to_string h in
  print_endline s;
  let c = Ribosome.generate_rna h in
  let sc = Ribosome.helix_to_string c in
  print_endline sc;
  let plausible_arn = Ribosome.generate_plausible_rna 15 in
  print_endline (Ribosome.helix_to_string plausible_arn);
  let prot = Ribosome.decode_arn plausible_arn in
  let sprot = Ribosome.string_of_protein prot in
  print_endline sprot