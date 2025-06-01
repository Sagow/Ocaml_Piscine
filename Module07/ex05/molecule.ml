class virtual molecule n (a : ((Atom.atom * int) list)) =
object (self)
  method private hill_sort atom_a atom_b =
    if (fst atom_a)#symbol = "C" then
      (-1)
    else if (fst atom_b)#symbol = "C" then
      1
    else if (fst atom_a)#symbol = "H" then
      (-1)
    else if (fst atom_b)#symbol = "H" then
      1
    else String.compare (fst atom_a)#symbol (fst atom_b)#symbol
  method name = n
  method atoms = List.sort self#hill_sort a
  method formula =
    let rec get_atoms l =
      match l with
      | h::t when (snd h) = 1-> (fst h)#symbol ^ (get_atoms t)
      | h::t -> (fst h)#symbol ^ (string_of_int (snd h)) ^ (get_atoms t)
      | [] -> ""
    in
    get_atoms self#atoms
  method to_string = "Molecule of " ^ self#name ^ ", Hill's formula : " ^ self#formula
  method equals (other : molecule) = self#name = other#name
end

class water =
object (self)
  inherit molecule "water" [(new Atom.hydrogen, 2); (new Atom.oxygen, 1)]
end

class carbon_dioxide =
object (self)
  inherit molecule "carbon dioxide" [(new Atom.carbon, 1); (new Atom.oxygen, 2)]
end

class carbon_monoxide =
object (self)
  inherit molecule "carbon monoxide" [(new Atom.carbon, 1); (new Atom.oxygen, 1)]
end

class soot =
object (self)
  inherit molecule "soot" [(new Atom.carbon, 1)]
end

class trinitrotoluene =
object (self)
  inherit molecule "trinitrotoluene" [(new Atom.nitrogen, 3); (new Atom.hydrogen, 5); (new Atom.oxygen, 6); (new Atom.carbon, 7)]
end

class iron3_oxide =
object (self)
  inherit molecule "iron(III) oxide" [(new Atom.iron, 2); (new Atom.oxygen, 3)]
end

class sodium_oxide =
object (self)
  inherit molecule "sodium oxide" [(new Atom.sodium, 2); (new Atom.oxygen, 1)]
end

class dioxygen =
object (self)
  inherit molecule "dioxygen" [(new Atom.oxygen, 2)]
end