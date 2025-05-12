let () =
 if Array.length Sys.argv = 2 then
  begin
    try
      begin
        let duration = int_of_string Sys.argv.(1) in
        if duration >= 0 then
          begin
            for d = 1 to duration do
              Unix.sleep 1
            done
          end
        else ()
      end
    with
      _ -> ()
  end
else ()