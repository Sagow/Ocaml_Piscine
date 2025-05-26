class people name =
  object (self)
    initializer print_endline ("Welcome to existence, " ^ name)
    val _name : string = name
    val mutable _hp = 100
    method to_string =
      _name ^ ", " ^ (string_of_int _hp )^ " hp left"
    method talk =
      print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
    method die =
      print_endline "Aaaarghh!"
    method lose_hp n =
      match n with
      | _ when n > 0 && n < _hp -> _hp <- _hp - n ; ()
      | _ when n > 0 -> _hp <- 0 ; self#die
      | _ -> ()
  end