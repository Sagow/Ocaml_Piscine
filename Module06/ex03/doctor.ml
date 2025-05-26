class doctor name =
  object (self)
    initializer print_endline "The Doctor is here"
    val _name : string = name
    val mutable _age = 150000
    val mutable _sidekick = new People.people "Rose"
    val mutable _hp = 100
    method to_string =
      "Doctor " ^ _name ^ ", " ^ (string_of_int _age )^ " years old, " ^ (string_of_int _hp )^ " hp left, and " ^ _sidekick#to_string
    method talk =
      print_endline "Hi! I’m the Doctor!"
    method travel_in_time s f =
      _age <- _age + (f - s);
      print_endline "              ___
      _______(_@_)_______
      | POLICE      BOX |
      |_________________|
       | _____ | _____ |
       | |###| | |###| |
       | |###| | |###| |   
       | _____ | _____ |   
       | || || | || || |
       | ||_|| | ||_|| |  
       | _____ |$_____ |  
       | || || | || || |  
       | ||_|| | ||_|| | 
       | _____ | _____ |
       | || || | || || |   
       | ||_|| | ||_|| |         
       |       |       |        
       *****************"
    method use_sonic_screwdriver =
      print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method private regenerate =
      _hp <- 100
    method lose_hp n =
      match n with
      | _ when n > 0 && n < _hp -> _hp <- _hp - n ; ()
      | _ when n > 0 -> _hp <- 0 ; ()
      | _ -> self#regenerate ; ()
    method get_name =
      _name
    method autogen =
      new doctor (string_of_int (Random.int 100) ^"th")
  end