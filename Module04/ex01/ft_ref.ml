type 'a ft_ref =
 {
  mutable content : 'a
 }

let return c =
  {content = c}

let get r =
  r.content

let set r c =
  r.content <- c

let bind r f =
  f r.content

let () =
 let my_ref = return 1 in 
 Printf.printf "currently containing %d \n" (get my_ref);
 Printf.printf "setting to 2 \n";
 set my_ref 2;
 Printf.printf "currently containing %d \n" (get my_ref);
 Printf.printf "if squared -> %d \n" (bind my_ref (fun x -> x*x))

