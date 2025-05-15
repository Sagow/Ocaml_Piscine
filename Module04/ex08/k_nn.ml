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

let comp_first a b =
  if (a < b) then -1
  else if (a > b) then 1
  else 0

let eu_dist a b =
  let comb = Array.combine a b in
  let res = ref 0.0 in
  for i = 0 to (Array.length comb - 1) do
    res := !res +. (Float.pow ((fst comb.(i)) -. (snd comb.(i))) 2.0)
  done;
  Float.sqrt (!res)

let k_nn (rl: (float array * string) list) k r =
  let getMostCommon (sl: (float * string) list) =
    let splitsl = List.split sl in
    let uniques = List.sort_uniq comp_first (snd splitsl) in
    let rec counting_letter uniques all =
      match uniques with
      | h::t -> (h, List.length (List.filter (fun x->x=h) all)) :: counting_letter t all
      | _ -> []
    in 
    let sortcommon a b =
      if (snd a > snd b) then -1
      else if (snd a < snd b) then -1
      else 0
    in
    let ranked_occ = List.sort sortcommon (counting_letter uniques (snd splitsl)) in
    fst (List.nth ranked_occ 0)
    (* let maxoccurrence = snd (List.nth ranked_occ 0) in
    let winnersl = List.find (fun x-> snd x = maxoccurrence) ranked_occ in
    fst (winnersl) *)
        
  in
  let distancel = ref [] in
  for i = 0 to (List.length rl) - 2 do
    distancel := (eu_dist (fst r) (fst (List.nth rl i)), snd (List.nth rl i)) :: !distancel
  done;
  let sortedKDist = take k (List.sort comp_first !distancel) in
  getMostCommon sortedKDist
  
let () =
  if Array.length Sys.argv <> 2 then
    print_endline "Usage : ./ex08 file.csv"
  else
    try
      begin
        let content = read Sys.argv.(1) in
        let res = examples_of_file content in
        let rA = ([|0.0;0.0;1.0;-1.0;0.0;0.0;0.0;0.0;1.0;1.0;1.0;-1.0;-0.71875;1.0;0.0;0.0;-1.0;1.0;1.0;1.0;-1.0;1.0;1.0;0.56250;-1.0;1.0;1.0;1.0;1.0;-1.0;1.0;1.0;1.0;1.0|],"") in
        Printf.printf "Prevision for A : %s\n" (k_nn res 2 rA);
      end
    with
    _ -> print_endline "Error"

    