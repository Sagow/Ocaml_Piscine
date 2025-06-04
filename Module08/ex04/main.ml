let () =
  let voyels = Myset.Set.returnlist ['a'; 'e'; 'i'; 'o'; 'u'; 'y'] in
  Myset.Set.printset voyels Char.escaped;
  let hello = Myset.Set.returnlist ['h'; 'e'; 'l'; 'l'; 'o'] in
  Myset.Set.printset hello Char.escaped;
  let abcdef = Myset.Set.returnlist ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] in
  Myset.Set.printset abcdef Char.escaped;

  Printf.printf "\nhello bind (char -> int)\n";
  let to_int = Myset.Set.bind hello Char.code in
  Myset.Set.printset to_int string_of_int;

  Printf.printf "\nhello union voyels\n";
  let union = Myset.Set.union hello voyels in
  Myset.Set.printset union Char.escaped;

  Printf.printf "\nhello inter voyels\n";
  let inter = Myset.Set.inter hello voyels in
  Myset.Set.printset inter Char.escaped;

  Printf.printf "\nhello diff voyels\n";
  let diff = Myset.Set.diff hello voyels in
  Myset.Set.printset diff Char.escaped;

  Printf.printf "\nabcdef filter (char -> int)mod2 = 0\n";
  let filtered = Myset.Set.filter abcdef (fun x -> (Char.code x) mod 2 = 0) in
  Myset.Set.printset filtered Char.escaped;

  Printf.printf "\nabcdef foreach uppercase_ascii = 0\n";
  let uppercased = Myset.Set.foreach abcdef Char.uppercase_ascii in
  Myset.Set.printset uppercased Char.escaped;

  let for_alled1 = Myset.Set.for_all abcdef (fun x -> if Char.code x < Char.code 'j' then true else false) in
  Printf.printf "\nabcdef for_all code x < code 'j' => %s\n" (string_of_bool for_alled1);

  let for_alled2 = Myset.Set.for_all voyels (fun x -> if Char.code x < Char.code 'j' then true else false) in
  Printf.printf "\nvoyels for_all code x < code 'j' => %s\n" (string_of_bool for_alled2);

  let exists = Myset.Set.exists voyels (fun x -> if Char.code x < Char.code 'j' then true else false) in
  Printf.printf "\nvoyels exists code x < code 'j' => %s\n" (string_of_bool exists)