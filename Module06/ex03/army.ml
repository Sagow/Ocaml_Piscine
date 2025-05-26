class ['a] army (autogeneration : unit -> 'a) =
  object (self)
    initializer print_endline ("A new army appears in the distance...\n")
    val mutable _members : 'a list = []
    method add =
      _members <- autogeneration () :: _members
    method delete =
      match _members with
      | h::t -> _members <- t
      | _ -> ()
    method in_rows =
      let rec salute members =
        match members with
        | h::t -> print_endline (h#get_name ^ ", ready to fight!"); salute t
        | _ -> ()
      in
      salute _members
    method counting_troops =
      let rec count members c =
        match members with
        | h::t -> count t (c + 1)
        | _ -> c
      in
      count _members 0
    method enlist n =
      let rec loop n =
        if n > 0 then
          begin
            self#add;
            loop (n - 1)
          end
        else ()
      in
      loop n
    method decimate n =
      let rec loop n =
        if n > 0 then
          begin
            self#delete;
            loop (n - 1)
          end
        else ()
      in
      loop n
  end