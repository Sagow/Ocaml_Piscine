let encode l =
  let rec countletter l c =
    match l with
    | head :: queue when head = c -> 1 + countletter queue c
    | _ -> 0
  in
  let rec getnextletter l c =
    match l with
    | head :: queue when head = c -> getnextletter queue c
    | head :: queue -> l
    | [] -> []
  in
  let rec loop l =
    match l with
    | [] -> []
    | c :: queue -> ((countletter l c) , c) :: (loop (getnextletter l c))
  in
  loop l

