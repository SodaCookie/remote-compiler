let rec a b =>
  switch b {
  | 0 => print_endline "End"
  | x =>
    print_endline @@ "Num " ^ string_of_int x;
    a (x - 1)
  };

a 100;
