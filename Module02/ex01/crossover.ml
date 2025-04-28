let crossover l1 l2 =
  let rec isin e l =
    match l with
    | head::queue when head = e -> true
    | head::queue -> isin e queue
    | [] -> false
  in
  let rec loop l1 l2 =
    match l1 with
    | head::queue ->
      if (isin head l2) && not (isin head queue) then head :: (loop queue l2)
      else loop queue l2
    | [] -> []
  in
  loop l1 l2