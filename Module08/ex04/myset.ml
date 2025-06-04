module Set = struct

  type 'a t = 'a list

  let return a = [a]
  let returnlist (a : 'a list) = a

  let rec bind a f =
    match a with
      | h::t -> (f h)::bind t f
      | [] -> []

  let union a b = a@b

  let inter a b =
    let rec is_in e l =
      match l with
      | h::t when h = e -> true
      | h::t -> is_in e t
      | [] -> false
    in
    let rec loop a b =
      match a with
      | h::t when is_in h b -> h::loop t b
      | h::t -> loop t b
      | [] -> []
    in 
    loop a b

  let diff a b =
    let rec is_in e l =
      match l with
      | h::t when h = e -> true
      | h::t -> is_in e t
      | [] -> false
    in
    let rec loop a b =
      match a with
      | h::t when is_in h b -> loop t b
      | h::t -> h::loop t b
      | [] -> []
    in 
    loop a b

  let filter a f =
    let rec loop a f =
      match a with
      | h::t when f h -> h::loop t f
      | h::t -> loop t f
      | [] -> []
    in 
    loop a f

  let foreach a f =
    let rec loop a f =
      match a with
      | h::t -> (f h)::loop t f
      | [] -> []
    in 
    loop a f

  let for_all a f =
    let rec loop a f =
      match a with
      | h::t when f h -> loop t f
      | h::t -> false
      | [] -> true
    in 
    loop a f

  let exists a f =
    let rec loop a f =
      match a with
      | h::t when f h -> true
      | h::t -> loop t f
      | [] -> false
    in 
    loop a f

  let printset a f =
    let rec loop a f =
      match a with
      | h::t -> Printf.printf "%s " (f h); loop t f
      | [] -> Printf.printf "\n"
    in 
    loop a f
end

