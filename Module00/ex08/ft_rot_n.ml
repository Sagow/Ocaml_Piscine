let rot n c =
  let new_c = int_of_char c + n in
  match c with 
  | _ when c >= 'a' && c <= 'z' -> begin
    if new_c > int_of_char 'z' then char_of_int (new_c - 26)
    else char_of_int (new_c)
  end
  | _ when c >= 'A' && c <= 'Z' -> begin
    if new_c > int_of_char 'Z' then char_of_int (new_c - 26)
    else char_of_int (new_c)
  end
  | _ -> c

let ft_rot_n n s =
  String.map (rot n) s