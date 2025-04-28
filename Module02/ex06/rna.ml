type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide =
  {
    p : phosphate;
    d : deoxyribose;
    n : nucleobase
  }
type helix = nucleotide list

let generate_nucleotide t = {
    p = "phosphate";
    d = "deoxyribose";
    n = match t with
    | 'A'|'a' -> A
    | 'T'|'t' -> T
    | 'C'|'c' -> C
    | 'G'|'g' -> G
    | 'U'|'u' -> U
    | _ -> None
}

let rec generate_helix n =
  Random.self_init ();
  if n <= 0 then []
  else 
    {p = "phosphate";
    d = "deoxyribose";
    n = match (Random.int 4) with
    | 0 -> A
    | 1 -> T
    | 2 -> C
    | _ -> G
    } :: (generate_helix (n-1))

let rec helix_to_string h =
  let getNucleobase n =
    match n.n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | _ -> "Error"
  in
  match h with
  | [] -> ""
  | head::queue -> (getNucleobase head) ^ (helix_to_string queue)

let rec generate_rna h =
  match h with
  | [] -> []
  | head::queue ->
    match head.n with
    | A -> (generate_nucleotide 'u') :: (generate_rna queue)
    | T -> (generate_nucleotide 'a') :: (generate_rna queue)
    | C -> (generate_nucleotide 'g') :: (generate_rna queue)
    | G -> (generate_nucleotide 'c') :: (generate_rna queue)
    | _ -> (generate_nucleotide ' ') :: (generate_rna queue)