let rec ft_power a b =
  if b=0 then
    1
  else a * ft_power a (b-1)
