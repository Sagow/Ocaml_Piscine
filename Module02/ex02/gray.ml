let gray n =
  let rec getbinary n =
    if n > 0 then
      begin
        let next = getbinary (n / 2) in
        match n mod 2 with
        | 0 -> next ^ "0"
        | _ -> next ^ "1"
      end
    else "0"
  in
  let rec xorall a b =
    if String.length a = 0 then ""
    else if (a.[0] = b.[0]) then "0" ^ (xorall (String.sub a 1 ((String.length a)-1)) (String.sub b 1 ((String.length b)-1)))
    else "1" ^ (xorall (String.sub a 1 ((String.length a)-1)) (String.sub b 1 ((String.length b)-1)))
  in
  let decal s =
    "0" ^ (String.sub s 0 ((String.length s) - 1))
  in
  let rec padding s length =
    if (String.length s) < length then padding ("0" ^ s) length
    else s
  in
  let rec printing current length =
    let bin = getbinary current in
    if (String.length (getbinary (current + 1)) > length) then xorall bin (decal bin)
    else (padding (xorall bin (decal bin)) length) ^ " " ^ (printing (current + 1) length)
  in
    
  print_endline (printing 0 n)