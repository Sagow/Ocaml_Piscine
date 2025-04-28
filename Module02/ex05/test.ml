

let () =
  let h = Helix.generate_helix 6 in
  let s = Helix.helix_to_string h in
  print_endline s;
  let c = Helix.complementary_helix h in
  let sc = Helix.helix_to_string c in
  print_endline sc;