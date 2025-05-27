class virtual alkane n num =
object (self)
  method name = n
  method atoms = [(new Atom.carbon, num); (new Atom.hydrogen, (num * 2 + 2))]
  method formula =
    let rec get_atoms l =
      match l with
      | h::t when (snd h) = 1-> (fst h)#symbol ^ (get_atoms t)
      | h::t -> (fst h)#symbol ^ (string_of_int (snd h)) ^ (get_atoms t)
      | [] -> ""
    in
    get_atoms self#atoms
  method to_string = "Alkane " ^ self#name ^ ", Hill's formula : " ^ self#formula
  method equals (other : alkane) = self#name = other#name
end

class methane =
object
  inherit alkane "methane" 1
end

class ethane =
object
  inherit alkane "ethane" 2
end

class octane =
object
  inherit alkane "octane" 8
end