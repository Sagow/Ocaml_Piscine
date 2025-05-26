
let human_army_demo () =
  let human_leader = new People.people "Humanest Human" in
  let human_army = new Army.army (fun () -> human_leader#autogen) in
  Printf.printf "The human army currently has %d soldiers\n" human_army#counting_troops;
  Printf.printf "Enlisting 20 soldiers\n";
  human_army#enlist 20;
  Printf.printf "The human army currently has %d soldiers\n" human_army#counting_troops;
  human_army#in_rows;
  Printf.printf "Decimating 10 soldiers\n";
  human_army#decimate 10;
  Printf.printf "The human army currently has %d soldiers\n" human_army#counting_troops;
  human_army#in_rows;
  Printf.printf "Decimating 100 soldiers\n";
  human_army#decimate 100;
  Printf.printf "The human army currently has %d soldiers\n" human_army#counting_troops;
  human_army#in_rows

let doctor_army_demo () =
  let doctor_leader = new Doctor.doctor "0th" in
  let doctor_army = new Army.army (fun () -> doctor_leader#autogen) in
  Printf.printf "The doctor army currently has %d soldiers\n" doctor_army#counting_troops;
  Printf.printf "Enlisting 20 soldiers\n";
  doctor_army#enlist 20;
  Printf.printf "The doctor army currently has %d soldiers\n" doctor_army#counting_troops;
  doctor_army#in_rows;
  Printf.printf "Decimating 10 soldiers\n";
  doctor_army#decimate 10;
  Printf.printf "The doctor army currently has %d soldiers\n" doctor_army#counting_troops;
  doctor_army#in_rows;
  Printf.printf "Decimating 100 soldiers\n";
  doctor_army#decimate 100;
  Printf.printf "The doctor army currently has %d soldiers\n" doctor_army#counting_troops;
  doctor_army#in_rows

let dalek_army_demo () =
  let dalek_leader = new Dalek.dalek in
  let dalek_army = new Army.army (fun () -> dalek_leader#autogen) in
  Printf.printf "The dalek army currently has %d soldiers\n" dalek_army#counting_troops;
  Printf.printf "Enlisting 20 soldiers\n";
  dalek_army#enlist 20;
  Printf.printf "The dalek army currently has %d soldiers\n" dalek_army#counting_troops;
  dalek_army#in_rows;
  Printf.printf "Decimating 10 soldiers\n";
  dalek_army#decimate 10;
  Printf.printf "The dalek army currently has %d soldiers\n" dalek_army#counting_troops;
  dalek_army#in_rows;
  Printf.printf "Decimating 100 soldiers\n";
  dalek_army#decimate 100;
  Printf.printf "The dalek army currently has %d soldiers\n" dalek_army#counting_troops;
  dalek_army#in_rows

let () =
  (* human_army_demo () *)
  (* doctor_army_demo () *)
  dalek_army_demo ()