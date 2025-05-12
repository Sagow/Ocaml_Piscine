let jokes = [|
  "I made a great deal buying metal, I guess you can call it a steal";
  "What can a bat to, a cat can't? Print the name of the file";
  "What do you call a pile of cats? A meow-ntain.";
  "Time flies like an arrow. Fruit flies like a banana.";
  "What’s red and bad for your teeth? A brick."
|]

let () =
  Random.self_init ();
  let i = Random.int (Array.length jokes) in
  print_endline (Array.get jokes i)