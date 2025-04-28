
let print_nucleotide (n : Nucleotides.nucleotide) =
  print_endline n.p;
  print_endline n.d;
  match n.n with
  | A -> print_endline "a"
  | T -> print_endline "t"
  | C -> print_endline "c"
  | G -> print_endline "g"
  | _ -> print_endline "-"

let () =
  print_endline "--------a--------";
  print_nucleotide (Nucleotides.generate_nucleotide('a'));
  print_endline "--------A--------";
  print_nucleotide (Nucleotides.generate_nucleotide('A'));
  print_endline "--------C--------";
  print_nucleotide (Nucleotides.generate_nucleotide('C'));
  print_endline "--------g--------";
  print_nucleotide (Nucleotides.generate_nucleotide('g'));
  print_endline "--------B--------";
  print_nucleotide (Nucleotides.generate_nucleotide('B'));
  print_endline "--------K--------";
  print_nucleotide (Nucleotides.generate_nucleotide('K'));
  print_endline "--------G--------";
  print_nucleotide (Nucleotides.generate_nucleotide('G'));
  print_endline "--------i--------";
  print_nucleotide (Nucleotides.generate_nucleotide('i'));