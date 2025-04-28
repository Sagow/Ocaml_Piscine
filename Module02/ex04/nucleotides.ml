type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide =
  {
    p : phosphate;
    d : deoxyribose;
    n : nucleobase
  }

  let generate_nucleotide t = {
    p = "phosphate";
    d = "deoxyribose";
    n = match t with
    | 'A'|'a' -> A
    | 'T'|'t' -> T
    | 'C'|'c' -> C
    | 'G'|'g' -> G
    | _ -> None
}