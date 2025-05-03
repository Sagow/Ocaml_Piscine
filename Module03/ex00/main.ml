let rec print_list l =
  match l with
  | h::t -> begin
    print_endline (Color.toString h ^ " or " ^ Color.toStringVerbose h);
    print_list t
  end
  | _ -> ()

let () =
  let list = Color.all in
  print_list list