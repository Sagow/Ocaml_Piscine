let rec take n l =
  if n <= 0 then []
  else match l with
  | h :: t -> h :: take (n - 1) t
  | [] -> []

let examples_of_line s =
  let l = String.split_on_char ',' s in 
  let vsl = take ((List.length l) - 1) l in
  try
    begin
      let vfl = List.map float_of_string vsl in
      let va = Array.of_list vfl in 
      (va, List.hd (List.rev l))
    end
  with
    _ -> ([||], "")

let examples_of_file s =
  let linel = String.split_on_char '\n' s in
  let res = ref [] in 
  for i = 0 to List.length linel - 1 do
    res := (examples_of_line (List.nth linel i)) :: !res
  done;
  List.rev !res

let print_res r =
  let print_pair p =
    let print_array_elem a =
      Printf.printf "%f " a
    in
    Array.iter print_array_elem (fst p);
    Printf.printf "%s\n" (snd p)
  in
  List.iter print_pair r

let read file =
  let ic = open_in file in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let () =
  if Array.length Sys.argv <> 2 then
    print_endline "Usage : ./ex06 name_of_file.csv"
  else
    try
      begin
        let content = read Sys.argv.(1) in
        let res = examples_of_file content in
        print_res res
      end
    with
    _ -> print_endline "Error"

    