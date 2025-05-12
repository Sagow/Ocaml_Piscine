
let getJokes file =
  let lignes = ref [] in
  let f = open_in file in
  try
    while true do
      lignes := input_line f :: !lignes
    done;
    assert false (* Jamais atteint *)
  with End_of_file ->
    close_in f;
    Array.of_list !lignes

let () =
  if (Array.length Sys.argv) = 2 then 
    begin
      let jokes = getJokes Sys.argv.(1) in
      Random.self_init ();
      try
        begin
          let i = Random.int (Array.length jokes) in
          print_endline (Array.get jokes i)
        end
      with _ -> ()
    end
  else ()