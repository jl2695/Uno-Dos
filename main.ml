let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Uno";
  print_endline "\nEnter the number of players.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | num_players -> ()

let () = main ()
