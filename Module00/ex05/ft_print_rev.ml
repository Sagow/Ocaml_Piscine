let rec print_letter_i s i =
 if i >= 0 then
  begin
    print_char(String.get s i);
    if i > 0 then print_letter_i s (i - 1)
    else print_char('\n')
  end

let ft_print_rev s =
  print_letter_i s (String.length(s) - 1)