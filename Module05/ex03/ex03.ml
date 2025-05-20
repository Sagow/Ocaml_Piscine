module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
  end

module type FRACTIONAL_BITS = sig
  val bits : int
end

module type MAKE =
    functor (Input : FRACTIONAL_BITS) -> FIXED

module Make : MAKE =
functor (Input : FRACTIONAL_BITS) ->
  struct
    type t = int
    let bits = Input.bits
    let of_int (n:int) : t =
      n lsl bits
    let of_float (f:float) : t =
      int_of_float (floor (0.5 +. (f *. (float_of_int (of_int 1)))))
    let to_float (t:t) : float =
      float_of_int t /. float_of_int (of_int 1)
    let to_int (t:t) : int =
      t lsr bits
    let to_string (t:t) : string =
      let tfl = to_float t in
      let tint = to_int t in
      if tfl = 0. || tfl /. float_of_int tint = 1.0 then string_of_int tint
      else string_of_float tfl
    let zero =
      of_int 0
    let one =
      of_int 1
    let succ t =
      t + 1
    let pred t =
      t - 1
    let min a b =
      if a <= b then a
      else b
    let max a b =
      if a >= b then a
      else b
    let gth a b =
      if a > b then true
      else false
    let lth a b =
      if a < b then true
      else false
    let gte a b =
      if a >= b then true
      else false
    let lte a b =
      if a <= b then true
      else false
    let eqp a b =
      if a == b then true
      else false (* physical equality *)
    let eqs a b =
      if a = b then true
      else false (* structural equality *) 
    let add a b =
      a + b
    let sub a b =
      a - b
    let mul a b =
      a * b
    let div a b =
      a / b
    let rec foreach st ed f =
      match st with
        | x when eqs x ed -> f st
        | _ -> f st ; foreach (succ st) ed f
  end


module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
let x8 = Fixed8.of_float 21.10 in
let y8 = Fixed8.of_float 21.32 in
let r8 = Fixed8.add x8 y8 in
print_endline (Fixed8.to_string r8);
Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
Printf.printf "max of %f and %f is %f, min is %f\n" (Fixed8.to_float x8) (Fixed8.to_float y8) (Fixed8.to_float (Fixed8.max x8 y8)) (Fixed8.to_float (Fixed8.min x8 y8));
let z8 = Fixed8.of_int 4 in
Printf.printf "int %d -> %s and reverse back to %d\n" 4 (Fixed8.to_string z8) (Fixed8.to_int z8);
Printf.printf "%s + %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Fixed8.to_string (Fixed8.add x8 y8));
Printf.printf "%s - %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Fixed8.to_string (Fixed8.sub x8 y8));
Printf.printf "%s * %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Fixed8.to_string (Fixed8.mul x8 y8));
Printf.printf "%s / %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Fixed8.to_string (Fixed8.div x8 y8));
Printf.printf "%s gth %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Bool.to_string (Fixed8.gth x8 y8));
Printf.printf "%s gte %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Bool.to_string (Fixed8.gte x8 y8));
Printf.printf "%s lth %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Bool.to_string (Fixed8.lth x8 y8));
Printf.printf "%s lte %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string y8) (Bool.to_string (Fixed8.lte x8 y8));
Printf.printf "%s eqs %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string x8) (Bool.to_string (Fixed8.eqs x8 x8));
Printf.printf "%s eqp %s = %s\n" (Fixed8.to_string x8) (Fixed8.to_string x8) (Bool.to_string (Fixed8.eqp x8 x8));
Printf.printf "%s pred = %s\n" (Fixed8.to_string x8) (Fixed8.to_string (Fixed8.pred x8));
Printf.printf "%s succ = %s\n" (Fixed8.to_string x8) (Fixed8.to_string (Fixed8.succ x8));
Printf.printf "zero = %s\n" (Fixed8.to_string Fixed8.zero);
Printf.printf "one = %s\n" (Fixed8.to_string Fixed8.one);
Fixed8.foreach (Fixed8.zero) (Fixed8.of_float 0.3) (fun x -> print_endline(Fixed8.to_string x))
