let eu_dist a b =
  let comb = Array.combine a b in
  let res = ref 0.0 in
  for i = 0 to (Array.length comb - 1) do
    res := !res +. (Float.pow ((fst comb.(i)) -. (snd comb.(i))) 2.0)
  done;
  Float.sqrt (!res)
  
let () =
  let pointA = [|0.0; 0.0; 0.0|] in
  let pointB = [|1.0; 1.0; 1.0|] in
  let pointC = [|4.2; -3.6; 10.4|] in
  Printf.printf "AB = %f\n" (eu_dist pointA pointB);
  Printf.printf "AC = %f\n" (eu_dist pointA pointC);
  Printf.printf "BC = %f\n" (eu_dist pointB pointC)