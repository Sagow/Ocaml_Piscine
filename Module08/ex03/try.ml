module Try = struct
  type 'a t =
  | Success of 'a
  | Failure of exn
  let return a = Success a
  let bind a f =
    match a with
      | Success s ->
        begin
          try f s 
          with ee -> Failure ee
        end
      | Failure e -> Failure e
  let recover a f  =
    match a with
    | Success s -> a
    | Failure e -> f e
  let filter a f =
    match a with
    | Success s when (f s) = false -> Failure (Failure "false")
    | Success s -> a
    | Failure e -> Failure e
  let flatten a =
    match a with
    | Success s ->
        begin
          match s with
          | Success ss -> s
          | Failure e -> Failure e
        end
    | Failure e -> Failure e
end

let print_int_try a =
  match a with
  | Try.Success s -> "Success (" ^ (string_of_int s) ^ ")"
  | Try.Failure e -> Printexc.to_string e

let print_float_try a =
  match a with
  | Try.Success s -> "Success (" ^ (string_of_float s) ^ ")"
  | Try.Failure e -> Printexc.to_string e
  
let print_string_try a =
  match a with
  | Try.Success s -> "Success (" ^ s ^ ")"
  | Try.Failure e -> Printexc.to_string e

let () =
Printf.printf "%d becomes %s\n" 3 (print_int_try (Try.return 3));
Printf.printf "%d bind x->hello %s\n" 0 (print_string_try(Try.bind (Try.return 0) (fun x -> Try.return "hello")));
Printf.printf "%f bind x->5/x %s\n" 0. (print_int_try(Try.bind (Try.return 0.) (fun x -> Try.return (5 / (int_of_float x)))));
Printf.printf "%d is even %s\n" 4 (print_int_try (Try.filter (Try.return 4) (fun x -> if x mod 2 = 0 then true else false)));
Printf.printf "%d is even %s\n" 3 (print_int_try (Try.filter (Try.return 3) (fun x -> if x mod 2 = 0 then true else false)));
let failureval = Try.filter (Try.return 4) (fun x -> false) in
Printf.printf "%s recover string %s\n" (print_int_try failureval) (print_int_try (Try.recover failureval (fun x -> Try.return (String.length (Printexc.to_string x)))));
Printf.printf "Success(Failure) flattens to %s\n" (print_int_try (Try.flatten (Try.return failureval)))
