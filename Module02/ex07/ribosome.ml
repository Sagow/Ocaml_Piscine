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

let rec generate_bases_triplets rna_list =
  match rna_list with
  | one::two::three::queue -> (one.n, two.n, three.n)::(generate_bases_triplets queue)
  | _ -> []

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val | None
type protein = aminoacid list

let rec generate_plausible_bases_triplets n =
  let random_aminoacid () =
    Random.self_init ();
    match Random.int 20 with
    | 0 -> (G, C, A) :: (G, C, C) :: (G, C, G) :: (G, C, U) :: []
    | 1 -> (A, G, A) :: (A, G, G) :: (C, G, A) :: (C, G, C) :: (C, G, G) :: (C, G, U) :: []
    | 2 -> (A, A, C) :: (A, A, U) :: []
    | 3 -> (G, A, C) :: (G, A, U) :: []
    | 4 -> (U, G, C) :: (U, G, U) :: []
    | 5 -> (C, A, A) :: (C, A, G) :: []
    | 6 -> (G, A, A) :: (G, A, G) :: []
    | 7 -> (G, G, A) :: (G, G, C) :: (G, G, G) :: (G, G, U) :: []
    | 8 -> (C, A, C) :: (C, A, U) :: []
    | 9 -> (A, U, A) :: (A, U, C) :: (A, U, U) :: []
    | 10 -> (C, U, A) :: (C, U, C) :: (C, U, G) :: (C, U, U) :: (U, U, A) :: (U, U, G) :: []
    | 11 -> (A, A, A) :: (A, A, G) :: []
    | 12 -> [(A, U, G)]
    | 13 -> (U, U, C) :: (U, U, U) :: []
    | 14 -> (C, C, C) :: (C, C, A) :: (C, C, G) :: (C, C, U) :: []
    | 15 -> (U, C, A) :: (U, C, C) :: (U, C, G) :: (U, C, U) :: (A, G, U) :: (A, G, C) :: []
    | 16 -> (A, C, A) :: (A, C, C) :: (A, C, G) :: (A, C, U) :: []
    | 17 -> [(U, G, G)]
    | 18 -> (U, A, C) :: (U, A, U) :: []
    | _ -> (G, U, A) :: (G, U, C) :: (G, U, G) :: (G, U, U) :: []
  in
  let stop_aminoacid () =
    (U, A, A) :: (U, A, G) :: (U, G, A)::[]
  in
  if n > 0 then random_aminoacid () @ (generate_plausible_bases_triplets (n-1))
  else if n = 0 then (stop_aminoacid () @ (generate_plausible_bases_triplets (n-1)))
  else []

let generate_plausible_rna n =
  let rec arn_of_triplet triplet =
    let get_nucleotide nbase =
      {p = "phosphate"; d = "deoxyribose"; n = nbase}
    in
    match triplet with
    | (x, y ,z)::queue -> (get_nucleotide x)::(get_nucleotide y)::(get_nucleotide z)::(arn_of_triplet queue)
    | _ -> []
  in
  arn_of_triplet (generate_plausible_bases_triplets n)

let rec generate_protein bases_triplet_list =
  match bases_triplet_list with
  | (U, A, A)::(U, A, G)::(U, G, A)::queue -> Stop::(generate_protein queue)
  | (G, C, A) :: (G, C, C) :: (G, C, G) :: (G, C, U) :: queue -> Ala::(generate_protein queue)
  | (A, G, A) :: (A, G, G) :: (C, G, A) :: (C, G, C) :: (C, G, G) :: (C, G, U) :: queue -> Arg::(generate_protein queue)
  | (A, A, C) :: (A, A, U) :: queue -> Asn::(generate_protein queue)
  | (G, A, C) :: (G, A, U) :: queue -> Asp::(generate_protein queue)
  | (U, G, C) :: (U, G, U) :: queue -> Cys::(generate_protein queue)
  | (C, A, A) :: (C, A, G) :: queue -> Gln::(generate_protein queue)
  | (G, A, A) :: (G, A, G) :: queue -> Glu::(generate_protein queue)
  | (G, G, A) :: (G, G, C) :: (G, G, G) :: (G, G, U) :: queue -> Gly::(generate_protein queue)
  | (C, A, C) :: (C, A, U) :: queue -> His::(generate_protein queue)
  | (A, U, A) :: (A, U, C) :: (A, U, U) :: queue -> Ile::(generate_protein queue)
  | (C, U, A) :: (C, U, C) :: (C, U, G) :: (C, U, U) :: (U, U, A) :: (U, U, G) :: queue -> Leu::(generate_protein queue)
  | (A, A, A) :: (A, A, G) :: queue -> Lys::(generate_protein queue)
  | (A, U, G) :: queue -> Met::(generate_protein queue)
  | (U, U, C) :: (U, U, U) :: queue -> Phe::(generate_protein queue)
  | (C, C, C) :: (C, C, A) :: (C, C, G) :: (C, C, U) :: queue -> Pro::(generate_protein queue)
  | (U, C, A) :: (U, C, C) :: (U, C, G) :: (U, C, U) :: (A, G, U) :: (A, G, C) :: queue -> Ser::(generate_protein queue)
  | (A, C, A) :: (A, C, C) :: (A, C, G) :: (A, C, U) :: queue -> Thr::(generate_protein queue)
  | (U, G, G) :: queue -> Trp::(generate_protein queue)
  | (U, A, C) :: (U, A, U) :: queue -> Tyr::(generate_protein queue)
  | (G, U, A) :: (G, U, C) :: (G, U, G) :: (G, U, U) :: queue -> Val::(generate_protein queue)
  | [] -> []
  | _ :: queue -> None::(generate_protein queue)

let decode_arn arn_list =
  let bases_list = generate_bases_triplets arn_list in
  generate_protein bases_list

let rec string_of_protein list =
  let translate_aminoacid acid =
    match acid with
    | Stop -> "."
    | Ala -> "Alanine "
    | Arg -> "Arginine "
    | Asn -> "Asparagine "
    | Asp -> "Aspartique "
    | Cys -> "Cysteine "
    | Gln -> "Glutamine "
    | Glu -> "Glutamique "
    | Gly -> "Glycine "
    | His -> "Histidine "
    | Ile -> "Isoleucine "
    | Leu -> "Leucine "
    | Lys -> "Lysine "
    | Met -> "Methionine "
    | Phe -> "Phenylalanine "
    | Pro -> "Proline "
    | Ser -> "Serine "
    | Thr -> "Threonine "
    | Trp -> "Tryptophane "
    | Tyr -> "Tyrosine "
    | Val -> "Valine "
    | None -> "Erreur "
  in
  match list with
  | head::queue -> (translate_aminoacid head) ^ (string_of_protein queue)
  | _ -> ""
