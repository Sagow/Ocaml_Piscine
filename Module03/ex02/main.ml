let rec print_list l =
  match l with
  | h::t -> begin
    print_endline (Card.toString h ^ " or " ^ Card.toStringVerbose h);
    print_list t;
  end
  | _ -> ()

let print_compare c1 c2 =
  let res = Card.compare c1 c2 in
  if res = -1 then print_endline (Card.toString c1 ^ " is weaker than " ^ Card.toString c2)
  else if res = 1 then print_endline (Card.toString c1 ^ " is stronger than " ^ Card.toString c2)
  else print_endline (Card.toString c1 ^ " and " ^ Card.toString c2 ^ " are equal")

let () =
  print_list Card.all;

  let card1 = Card.newCard T4 Spade in
  let card2 = Card.newCard Queen Club in
  let card3 = Card.newCard T4 Diamond in
  let card4 = Card.newCard As Spade in
  let card5 = Card.newCard As Club in

  print_compare card1 card2;
  print_compare card1 card3;
  print_compare card2 card1;
  Printf.printf "%s and %s\n" (Card.toString card1) (Card.toString card2);
  print_endline ("Max : " ^ Card.toStringVerbose (Card.max card1 card2) ^ " | Min : " ^ Card.toStringVerbose (Card.min card1 card2));
  Printf.printf "%s and %s\n" (Card.toString card4) (Card.toString card5);
  print_endline ("Max : " ^ Card.toStringVerbose (Card.max card4 card5) ^ " | Min : " ^ Card.toStringVerbose (Card.min card4 card5));
  print_endline (Card.toStringVerbose (Card.best Card.allClubs) ^ " is the best");
  print_endline (Card.toStringVerbose card1 ^ "is a Spade : " ^ (string_of_bool (Card.isOf card1 Spade)));
  print_endline (Card.toStringVerbose card2 ^ "is a Spade : " ^ (string_of_bool (Card.isOf card2 Spade)));
  print_endline (Card.toStringVerbose card3 ^ "is a Diamond : " ^ (string_of_bool (Card.isDiamond card3)));
  print_endline (Card.toStringVerbose card2 ^ "is a Diamond : " ^ (string_of_bool (Card.isDiamond card2)));