let rec printList list =
  match list with
  | head :: queue ->
    Printf.printf "%d%c" (fst head) (snd head);
    printList queue
  | _ -> print_endline ""


let test list =
  printList (Encode.encode ['a';'a';'a';'a';'a']);
  printList (Encode.encode ['a';'b';'b';'b';'a']);
  printList (Encode.encode ['b';'a';'b';'a';'a']);
  printList (Encode.encode []);
  printList (Encode.encode ['b'; 'a'; 'a'; 'b'; 'b'; 'a'; 'b'; 'a'; 'a'; 'a'; 'b'; 'b'; 'a'; 'b'; 'a'; 'b'; 'b'; 'a'; 'a'; 'b'])

let () = test()