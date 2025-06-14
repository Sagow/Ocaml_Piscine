
module Value =
  struct

    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all =  T2::T3::T4::T5::T6::T7::T8::T9::T10::Jack::Queen::King::As::[]

    let toInt c =
      match c with
      | T2 -> 1
      | T3 -> 2
      | T4 -> 3
      | T5 -> 4
      | T6 -> 5
      | T7 -> 6
      | T8 -> 7
      | T9 -> 8
      | T10 -> 9
      | Jack -> 10
      | Queen -> 11
      | King -> 12
      | As-> 13

    let toString c =
      match c with
      | T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
      | As-> "A"

    let toStringVerbose c =
      match c with
      | T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "Jack"
      | Queen -> "Queen"
      | King -> "King"
      | As-> "As"

    let next c =
      match c with
      | T2 -> T3
      | T3 -> T4
      | T4 -> T5
      | T5 -> T6
      | T6 -> T7
      | T7 -> T8
      | T8 -> T9
      | T9 -> T10
      | T10 -> Jack
      | Jack -> Queen
      | Queen -> King
      | King -> As
      | As-> invalid_arg "trop grand"

    let previous c =
    match c with
      | T2 -> invalid_arg "trop petit"
      | T3 -> T2
      | T4 -> T3
      | T5 -> T4
      | T6 -> T5
      | T7 -> T6
      | T8 -> T7
      | T9 -> T8
      | T10 -> T9
      | Jack -> T10
      | Queen -> Jack
      | King -> Queen
      | As-> King

  end

module Color =
  struct
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
  end

type t = {
  color:Color.t;
  value:Value.t;
  }

let newCard v c =
  {
    value = v;
    color = c
  }

let allSpades =
  (newCard T2 Spade)::
  (newCard T3 Spade)::
  (newCard T4 Spade)::
  (newCard T5 Spade)::
  (newCard T6 Spade)::
  (newCard T7 Spade)::
  (newCard T8 Spade)::
  (newCard T9 Spade)::
  (newCard T10 Spade)::
  (newCard Jack Spade)::
  (newCard Queen Spade)::
  (newCard King Spade)::
  (newCard As Spade)::[]

let allHearts =
  (newCard T2 Heart)::
  (newCard T3 Heart)::
  (newCard T4 Heart)::
  (newCard T5 Heart)::
  (newCard T6 Heart)::
  (newCard T7 Heart)::
  (newCard T8 Heart)::
  (newCard T9 Heart)::
  (newCard T10 Heart)::
  (newCard Jack Heart)::
  (newCard Queen Heart)::
  (newCard King Heart)::
  (newCard As Heart)::[]

let allDiamonds =
  (newCard T2 Diamond)::
  (newCard T3 Diamond)::
  (newCard T4 Diamond)::
  (newCard T5 Diamond)::
  (newCard T6 Diamond)::
  (newCard T7 Diamond)::
  (newCard T8 Diamond)::
  (newCard T9 Diamond)::
  (newCard T10 Diamond)::
  (newCard Jack Diamond)::
  (newCard Queen Diamond)::
  (newCard King Diamond)::
  (newCard As Diamond)::[]
let allClubs =
  (newCard T2 Club)::
  (newCard T3 Club)::
  (newCard T4 Club)::
  (newCard T5 Club)::
  (newCard T6 Club)::
  (newCard T7 Club)::
  (newCard T8 Club)::
  (newCard T9 Club)::
  (newCard T10 Club)::
  (newCard Jack Club)::
  (newCard Queen Club)::
  (newCard King Club)::
  (newCard As Club)::[]

let all =
  allSpades @ allHearts @ allDiamonds @ allClubs

let getColor c =
  c.color

let getValue c =
  c.value

let toString c =
  Value.toString c.value ^ (Color.toString c.color)

let toStringVerbose c =
  "Card(" ^ Value.toStringVerbose c.value ^ "," ^ (Color.toStringVerbose c.color) ^ ")"

let compare c1 c2 =
  if (Value.toInt c1.value) = (Value.toInt c2.value) then 0
  else if (Value.toInt c1.value) < (Value.toInt c2.value) then -1
  else 1

let max c1 c2 =
  if (Value.toInt c1.value) < (Value.toInt c2.value) then c2
  else c1

let min c1 c2 =
  if (Value.toInt c1.value) > (Value.toInt c2.value) then c2
  else c1

let best list : t =
  match list with
    | h::t -> List.fold_left max h t
    | [] -> invalid_arg "Liste vide"

let isOf c color =
  if c.color = color then true
  else false

let isSpade c =
  if c.color = Spade then true
  else false

let isHeart c =
if c.color = Heart then true
else false

let isDiamond c =
if c.color = Diamond then true
else false

let isClub c =
  if c.color = Club then true
  else false