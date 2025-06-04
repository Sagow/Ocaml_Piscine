module Watchtower =
  struct
    type hour = int
    let zero = 12
    let add a b = (a + b) mod zero
    let sub a b =
      if a > b then (a - b) mod zero
      else (a mod zero) + zero - (b mod zero)
  end