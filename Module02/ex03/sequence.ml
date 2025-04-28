let sequence n =
  let rec printlist list =
    match list with
    | head::queue -> (string_of_int head) ^ (printlist queue)
    | _ -> ""
  in
  let rec getnext list i n =
    let rec count list elem =
      match list with
      | head::queue when head = elem -> 1 + (count queue elem)
      | _ -> 0
    in
    let rec getnewint list elem =
      match list with
      | head::queue when head = elem -> getnewint queue elem
      | _ -> list
    in
    let rec translate list =
      match list with
      | head::queue -> (count list head) :: head :: (translate (getnewint list head))
      | _ -> []
    in
    if n <= 0 then ""
    else if i < n then getnext (translate list) (i+1) n 
    else printlist list
  in
  if n >= 1 then print_endline (getnext [1] 1 n)
  else print_endline ""