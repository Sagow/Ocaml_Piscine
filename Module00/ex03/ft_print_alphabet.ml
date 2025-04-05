let rec print_letter n =
  if n > int_of_char ('z') then print_char('\n')
  else
    begin
      print_char(char_of_int(n));
      print_letter (n+1)
    end

let ft_print_alphabet () =
  print_letter (int_of_char 'a')