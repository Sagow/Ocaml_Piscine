let () =
  let doc = new Doctor.doctor "11th" in
  let randomPerson = new People.people "John Doe" in
  let dalek1 = new Dalek.dalek in

  dalek1#talk;
  dalek1#talk;
  dalek1#talk;
  print_endline "The Doctor attacks the dalek (60hp)";
  dalek1#lose_hp 60;
  print_endline dalek1#to_string;
  dalek1#exterminate randomPerson;
  print_endline randomPerson#to_string;
  print_endline dalek1#to_string;
  print_endline "The Doctor attacks the dalek (60hp)";
  dalek1#lose_hp 60;
  print_endline dalek1#to_string;
  print_endline "The dalek attacks the Doctor (30hp)";
  doc#lose_hp 30;
  print_endline doc#to_string;
  print_endline "The Doctor attacks the dalek (50hp)";
  dalek1#lose_hp 50;
