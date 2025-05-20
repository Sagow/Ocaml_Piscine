module StringHash =
  struct
    type t = string
    let equal i j = i=j
    let hash s = 
      (* djb2 *)
      let rec aux i hash =
        if i >= String.length s then hash
        else
          let c = Char.code s.[i] in
          aux (i + 1) ((hash * 33) + c)
      in
      aux 0 5381
  end
module StringHashtbl = Hashtbl.Make (StringHash)

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht