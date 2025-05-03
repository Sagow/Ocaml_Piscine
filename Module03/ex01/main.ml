let rec print_list l =
  match l with
  | h::t -> begin
    print_endline (Value.toString h ^ " or " ^ Value.toStringVerbose h);
    let _ = (* On ignore la valeur de 'next' si l'exception est levée *)
      try
        let previous = Value.previous h in
        print_endline ("previous : " ^ Value.toString previous);
        true
      with
      | Invalid_argument msg -> print_endline ("exception triggered:" ^ msg);
      false
    in
    let _ = (* On ignore la valeur de 'next' si l'exception est levée *)
      try
        let next = Value.next h in
        print_endline ("next : " ^ Value.toString next);
        true
      with
      | Invalid_argument msg -> print_endline ("exception triggered:" ^ msg);
      false
    in
    print_list t;
  end
  | _ -> ()

let () =
  print_list Value.all