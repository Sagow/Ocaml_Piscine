let rec check_letter s i =
  let size = String.length s in
  if (i < size) then
    (String.get s i) = String.get s (size - i - 1) && check_letter s (i+1)
  else true

let ft_is_palindrome s =
  check_letter s 0