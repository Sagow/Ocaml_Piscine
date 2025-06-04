module type MONOID =
sig
type element
val zero1 : element
val zero2 : element
val mul : element -> element -> element
val add : element -> element -> element
val div : element -> element -> element
val sub : element -> element -> element
end

module INT = struct
  type element = int
  (* addition and substraction *)
  let zero1 = 0
  (* multiplication & division *)
  let zero2 = 1
  let mul a b = a * b
  let add a b = a + b
  let div a b = a / b
  let sub a b = a - b
end

module FLOAT = struct
  type element = float
  (* addition and substraction *)
  let zero1 = 0.
  (* multiplication & division *)
  let zero2 = 1.
  let mul a b = a *. b
  let add a b = a +. b
  let div a b = a /. b
  let sub a b = a -. b
end

module Calc =
  functor (M : MONOID) ->
    struct
      let add a b = M.add a b
      let sub a b = M.sub a b
      let mul a b = M.mul a b
      let div a b = M.div a b
      let rec power a i =
        if i > 0 then M.mul a (power a (i - 1))
        else M.zero2
      let rec fact a =
        if a > M.zero2 then M.mul a (fact (M.sub a M.zero2))
        else a
    end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)
let () =
  Printf.printf "INT : 3 + 4 = %d\n" (Calc_int.add 3 4);
  Printf.printf "FLOAT : 3 + 4 = %f\n" (Calc_float.add 3. 4.);
  Printf.printf "INT : 3 - 4 = %d\n" (Calc_int.sub 3 4);
  Printf.printf "FLOAT : 3 - 4 = %f\n" (Calc_float.sub 3. 4.);
  Printf.printf "INT : 3 * 4 = %d\n" (Calc_int.mul 3 4);
  Printf.printf "FLOAT : 3 * 4 = %f\n" (Calc_float.mul 3. 4.);
  Printf.printf "INT : 3 / 4 = %d\n" (Calc_int.div 3 4);
  Printf.printf "FLOAT : 3 / 4 = %f\n" (Calc_float.div 3. 4.);
  Printf.printf "INT : 3 ^ 4 = %d\n" (Calc_int.power 3 4);
  Printf.printf "FLOAT : 3 ^ 4 = %f\n" (Calc_float.power 3. 4);
  Printf.printf "INT : 3 ! = %d\n" (Calc_int.fact 3);
  Printf.printf "FLOAT : 3 ! = %f\n" (Calc_float.fact 3.)