let is_digit c =
  c >= '0' && c <= '9'

let test () =
 print_endline(string_of_bool(Ft_string_all.ft_string_all is_digit "12345"));
 print_endline(string_of_bool(Ft_string_all.ft_string_all is_digit "1234a"));
 print_endline(string_of_bool(Ft_string_all.ft_string_all is_digit "z2341111234579087654345678987654345675"));
 print_endline(string_of_bool(Ft_string_all.ft_string_all is_digit ""))

let () = test()