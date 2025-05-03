let rec print_list l =
  match l with
  | h::t -> begin
    print_endline (Deck.Card.toString h ^ " or " ^ Deck.Card.toStringVerbose h);
    print_list t;
  end
  | _ -> ()

let print_compare c1 c2 =
  let res = Deck.Card.compare c1 c2 in
  if res = -1 then print_endline (Deck.Card.toString c1 ^ " is weaker than " ^ Deck.Card.toString c2)
  else if res = 1 then print_endline (Deck.Card.toString c1 ^ " is stronger than " ^ Deck.Card.toString c2)
  else print_endline (Deck.Card.toString c1 ^ " and " ^ Deck.Card.toString c2 ^ " are equal")

let () =
  print_list Deck.Card.all;

  let card1 = Deck.Card.newCard T4 Spade in
  let card2 = Deck.Card.newCard Queen Club in
  let card3 = Deck.Card.newCard T4 Diamond in

  print_compare card1 card2;
  print_compare card1 card3;
  print_compare card2 card1;
  print_endline ("Max : " ^ Deck.Card.toStringVerbose (Deck.Card.max card1 card2) ^ " | Min : " ^ Deck.Card.toStringVerbose (Deck.Card.min card1 card2));
  print_endline (Deck.Card.toStringVerbose (Deck.Card.best Deck.Card.allClubs) ^ " is the best");
  print_endline (Deck.Card.toStringVerbose card1 ^ "is a Spade : " ^ (string_of_bool (Deck.Card.isOf card1 Spade)));
  print_endline (Deck.Card.toStringVerbose card2 ^ "is a Spade : " ^ (string_of_bool (Deck.Card.isOf card2 Spade)));
  print_endline (Deck.Card.toStringVerbose card3 ^ "is a Diamond : " ^ (string_of_bool (Deck.Card.isDiamond card3)));
  print_endline (Deck.Card.toStringVerbose card2 ^ "is a Diamond : " ^ (string_of_bool (Deck.Card.isDiamond card2)));


  let deck = Deck.newDeck () in
  let rec print_string_list l =
    match l with
    | h::t -> print_endline h; print_string_list t
    | [] -> ()
  in
  print_string_list (Deck.toStringList deck);
  let deck2 = Deck.newDeck () in
  print_string_list (Deck.toStringListVerbose deck2);
  let rec draw_cards d =
    try
      let pair = Deck.drawCard d in
      print_endline (Deck.Card.toStringVerbose (fst pair));
      draw_cards (snd pair);
    with
      _ -> print_endline "end of the deck"
  in
  print_endline "drafting cards from the previous deck :";
  draw_cards deck2

