let rec printList list =
  match list with
  | head :: queue ->
    print_char head;
    printList queue
  | _ -> print_endline ""


let test l1 l2 =
  printList l1;
  print_endline "&";
  printList l2;
  print_endline "=";
  printList (Crossover.crossover l1 l2);
  print_endline ""

let myset () =
  let a = ['a'; 'a'; 'a'; 'a'; 'a'] in
  let b = [] in
  let c = ['a'; 'b';'c';'d';'e'] in
  let d = ['c';'a';'f';'b';'a'] in
  let e = ['f';'g';'h';'i';'j'] in
  test a b;
  test a c;
  test d a;
  test d e;
  test d c

let () = myset ()
  