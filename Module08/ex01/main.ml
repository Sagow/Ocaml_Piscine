let print_proj (p : App.App.project) =
  Printf.printf "The project %s has status %s and a mark of %d\n" (App.App.get_name p) (App.App.get_status p) (App.App.get_mark p) 

let () =
  let p1 : App.App.project = ("libft", "fail", 30) in
  print_proj p1;
  let p2 = App.App.fail p1 in
  print_proj p2;
  let p3 = App.App.success p1 in
  print_proj p3;
  let p4 = App.App.combine p1 p3 in
  print_proj p4