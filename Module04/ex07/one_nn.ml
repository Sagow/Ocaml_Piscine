type radar = (float array * string)

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

let eu_dist a b =
  let comb = Array.combine a b in
  let res = ref 0.0 in
  for i = 0 to (Array.length comb - 1) do
    res := !res +. (Float.pow ((fst comb.(i)) -. (snd comb.(i))) 2.0)
  done;
  Float.sqrt (!res)

let one_nn rl r =
  let comp_first a b =
    if (a < b) then -1
    else if (a > b) then 1
    else 0
  in
  let distancel = ref [] in
  for i = 0 to (List.length rl) - 1 do
    distancel := (eu_dist (fst r) (fst (List.nth rl i)), snd (List.nth rl i)) :: !distancel
  done;
  let sortedDist = List.sort comp_first !distancel in
  snd (List.hd sortedDist)
  
let () =
  if Array.length Sys.argv <> 1 then
    print_endline "Usage : ./ex07"
  else
    try
      begin
        let content = read "mytest.csv" in
        let res = examples_of_file content in
        print_res res;
        let rA = ([|1.29;2.319;0.2421|],'0') in
        let rB = ([|-1.43;-2.639;-1.242|],'0') in
        let rC = ([|1.1;2.07;-0.06|],'0') in
        Printf.printf "Prevision for A : %s\n" (one_nn res rA);
        Printf.printf "Prevision for B : %s\n" (one_nn res rB);
        Printf.printf "Prevision for C : %s\n" (one_nn res rC)
      end
    with
    _ -> print_endline "Error"

    