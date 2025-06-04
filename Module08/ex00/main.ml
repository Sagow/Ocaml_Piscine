let () =
  Printf.printf "%dh + %dh = %dh\n" 4 5 (Watchtower.Watchtower.add 4 5);
  Printf.printf "%dh + %dh = %dh\n" 13 5 (Watchtower.Watchtower.add 13 5);
  Printf.printf "%dh + %dh = %dh\n" 6 0 (Watchtower.Watchtower.add 6 0);
  Printf.printf "%dh - %dh = %dh\n" 4 5 (Watchtower.Watchtower.sub 4 5);
  Printf.printf "%dh - %dh = %dh\n" 34 5 (Watchtower.Watchtower.sub 34 5);
  Printf.printf "%dh - %dh = %dh\n" 6 0 (Watchtower.Watchtower.sub 6 0)