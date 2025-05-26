Random.self_init ();

class dalek =
  object (self)
    val mutable _name =
      let random_letter () =
        Char.chr ((Char.code 'a') + (Random.int 26))
      in
      "Dalek" ^
      String.make 1 (Char.uppercase_ascii (random_letter ())) ^
      String.make 1 (random_letter ()) ^
      String.make 1 (random_letter ())
    initializer print_endline (_name ^ " in approach")
    val mutable _hp = 100
    val mutable _shield = true
    val _typical_dalek_lines =
      [|"Explain! Explain!";
      "Exterminate! Exterminate!";
      "I obey!";
      "You are the Doctor! You are the enemy of the Daleks!"|]
    method private shield_state =
      if _shield = true then "on"
      else "off"
    method to_string =
      _name ^ ", " ^ (string_of_int _hp )^ " hp left, shield " ^ self#shield_state
    method talk =
      print_endline (Array.get _typical_dalek_lines (Random.int (Array.length _typical_dalek_lines)))
    method exterminate (person : People.people) =
      person#lose_hp 100;
      _shield <- not _shield
    method die =
      print_endline "Emergency Temporal Shift!"
    method lose_hp n =
      match n with
      | _ when _shield = true -> ()
      | _ when n > 0 && n < _hp -> _hp <- _hp - n ; ()
      | _ when n > 0 -> _hp <- 0 ; self#die
      | _ -> ()
    method get_name =
      _name
    method autogen =
      new dalek

  end

