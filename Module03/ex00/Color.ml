type t = Spade | Heart | Diamond | Club

let all = Spade::Heart::Diamond::Club::[]

let toString s =
  match s with
  | Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club -> "C"

let toStringVerbose s =
  match s with
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"