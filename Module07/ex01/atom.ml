class virtual atom n s an =
object (self)
  method name : string = n
  method symbol : string = s
  method atomic_number : int = an
  method to_string = self#name ^ " of symbol " ^ self#symbol ^ ", atomic number " ^ (string_of_int self#atomic_number)
  method equals (other : atom) = self#name = other#name
end

class hydrogen =
object (self)
  inherit atom "hydrogen" "H" 1
end

class carbon =
object (self)
  inherit atom "carbon" "C" 6
end

class oxygen =
object (self)
  inherit atom "oxygen" "O" 8
end

class nitrogen =
object (self)
  inherit atom "nitrogen" "N" 7
end

class sodium =
object (self)
  inherit atom "sodium" "Na" 11
end

class iron =
object (self)
  inherit atom "iron" "Fe" 26
end