let test () =
  print_endline(string_of_bool(Ft_is_palindrome.ft_is_palindrome("madam")));
  print_endline(string_of_bool(Ft_is_palindrome.ft_is_palindrome("madamm")));
  print_endline(string_of_bool(Ft_is_palindrome.ft_is_palindrome("maddam")));
  print_endline(string_of_bool(Ft_is_palindrome.ft_is_palindrome("aaaa")));
  print_endline(string_of_bool(Ft_is_palindrome.ft_is_palindrome("")))

let () = test()