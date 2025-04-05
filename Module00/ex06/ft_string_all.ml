let rec test_i f s i =
  if i = (String.length s) - 1 then f (String.get s i)
  else f (String.get s i) && test_i f s (i+1)


let ft_string_all f s =
 if (String.length s) > 0 then test_i f s 0
 else false