let test () =
  Ft_test_sign.ft_test_sign 1000;
  Ft_test_sign.ft_test_sign 1;
  Ft_test_sign.ft_test_sign 0;
  Ft_test_sign.ft_test_sign (-1);
  Ft_test_sign.ft_test_sign (-1000)

let () = test()