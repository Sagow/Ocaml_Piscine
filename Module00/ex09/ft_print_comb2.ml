let print_numbers a b =
  if a < 10 then print_char '0'
  else ();
  print_int a;
  print_char ' ';
  if b < 10 then print_char '0'
  else ();
  print_int b;
  if a = 98 && b = 99 then
    print_char '\n'
  else
    begin
      print_char ',';
      print_char ' ';
    end


let rec get_next_number a b =
  print_numbers a b;
  match b with
  | 99 -> begin
    if a < 98 then get_next_number (a + 1) (a + 2)
    else print_numbers a b
  end
  | _ -> get_next_number a (b + 1)

let ft_print_comb2 () =
  get_next_number 0 1