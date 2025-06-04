module App =
  struct
    type project = (string * string * int)
    let get_name (tuple : project) =
      match tuple with
      | (a,_,_) -> a 
    let get_status (tuple : project) =
      match tuple with
      | (_,a,_) -> a 
    let get_mark (tuple : project) =
      match tuple with
      | (_,_,a) -> a 
    let zero = ("", "", 0)
    let combine a b =
      let mark = ((get_mark a) + (get_mark b))/2 in 
      if mark >= 80 then (get_name a ^ get_name b, "succeed", mark)
      else (get_name a ^ get_name b, "fail", mark)
    let fail a =
      get_name a, "fail", 0
    let success a =
      get_name a, "succeed", 80
  end