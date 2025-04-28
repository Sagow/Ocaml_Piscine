

let () =
  let h = Rna.generate_helix 6 in
  let s = Rna.helix_to_string h in
  print_endline s;
  let c = Rna.generate_rna h in
  let sc = Rna.helix_to_string c in
  print_endline sc;