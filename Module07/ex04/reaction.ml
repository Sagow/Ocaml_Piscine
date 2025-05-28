let rec print_rec l =
  match l with
  | h::t -> print_endline ((fst h)#symbol ^ " * " ^ string_of_int(snd h)); print_rec t
  | [] -> ()

class virtual reaction s r =
object (self)
  method start : (Molecule.molecule * int) list = s
  method result : (Molecule.molecule * int) list = r
  method virtual get_start: (Molecule.molecule * int) list
  method virtual get_result: (Molecule.molecule * int) list
  method virtual balance: reaction
  method virtual is_balanced: bool
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
  method to_string =
    let rec get_molecules l needs_plus =
      match l with
      | h::t when needs_plus = true -> " + " ^ string_of_int (snd h) ^ " " ^ (fst h)#formula ^ (get_molecules t true)
      | h::t -> string_of_int (snd h) ^ " " ^ (fst h)#formula ^ (get_molecules t true)
      | [] -> ""
    in
    (get_molecules self#start false) ^ " -> " ^ (get_molecules self#result false)
end

class alkane_combustion s r =
object (self)
  inherit reaction s r as super
  method get_start = if self#is_balanced then super#start else failwith "unbalanced reaction"
  method get_result = if self#is_balanced then super#result else failwith "unbalanced reaction"
  method balance = 
    let rec test_values (s : (Molecule.molecule * int) list) (r : (Molecule.molecule * int) list) =
      let tested = new alkane_combustion s r in 
      if tested#is_balanced then tested
      else
        begin
          let mega_start = self#simplify (List.sort super#hill_sort (self#get_atoms s))
          and mega_result = self#simplify (List.sort super#hill_sort (self#get_atoms r)) in 
          (* print_rec mega_start;
          print_endline "----";
          print_rec mega_result;
          print_endline "====="; *)
          (* too much C *)
          if snd (List.hd mega_start) < snd (List.hd mega_result) then test_values (self#up_value s 0) r
          else if snd (List.hd mega_start) > snd (List.hd mega_result) then test_values s (self#up_value r 0)
          (* too much H *)
          else if snd (List.nth mega_start 1) < snd (List.nth mega_result 1) then test_values (self#up_value s 0) r
          else if snd (List.nth mega_start 1) > snd (List.nth mega_result 1) then test_values s (self#up_value r 1)
          (* too much O *)
          else if snd (List.nth mega_start 2) < snd (List.nth mega_result 2) then test_values (self#up_value s 1) r
          else
            begin
            print_rec mega_start;
            print_endline "----";
            print_rec mega_result;
            failwith "it should be balanced"
            end
        end
    in
    test_values super#start super#result
    (* new alkane_combustion super#start super#result *)

  method private up_value l i =
    let rec loop l i =
      match l with
      | h::t -> if i <> 0 then h::(loop t (i - 1)) else (fst h, (snd h) + 1):: (loop t (i - 1))
      |[] -> []
    in 
    loop l i

  method private get_atoms l =
    let rec get_atoms_rec l =
      match l with
      | h::t -> (List.map (fun (a, x) -> (a, x * (snd h))) (fst h)#atoms) @ get_atoms_rec t
      | [] -> []
    in
    get_atoms_rec l

  method private simplify l =
    let rec loop a count l =
      match l with
      | h::t when (fst h)#equals a -> loop a (count + (snd h)) t
      | h::t -> (a, count):: loop (fst h) (snd h) t
      | [] when count > 0 -> [(a, count)]
      | [] -> []
    in
    loop (fst (List.hd l)) 0 l
  
  method is_balanced =
    let listStart = List.sort super#hill_sort (self#get_atoms super#start)
    and listResult = List.sort super#hill_sort (self#get_atoms super#result) in
    (* print_rec listStart;
    print_endline "------";
    print_rec listResult;
    print_endline "======="; *)
    let listEqual a b =
      if (fst a)#equals (fst b) && (snd a) = (snd b) then true else false
    in
    let sStart = self#simplify listStart
    and sResult = self#simplify listResult in 
    (* print_rec sStart;
    print_endline "--------";
    print_rec sResult; *)
    List.equal listEqual sStart sResult
end