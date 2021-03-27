open State
open Person
open Command

let rec play_game n st =
  for i = 0 to Array.length (get_people st) - 1 do
    print_string (get_people st).(i).name
  done

let rec take_names name name_lst =
  take_names (read_line ()) (name :: name_lst)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Uno";
  print_endline "\nEnter the number of players.\n";
  print_string "> ";
  let names = [] in
  match parse (read_line ()) with
  | exception End_of_file -> ()
  | Name name -> take_names name names
  | Begin ->
      let len_names = List.length names in
      play_game len_names (init_state len_names)
  | Draw -> ()
  | Place card -> ()

(* FOR I IN RANGE NUM_PLAYERS PROMPT FOR NAME AND ADD TO ARRAY PASS INTO
   INIT *)
let () = main ()

(* ocamlbuild -pkgs ANSITerminal main.byte && ./main.byte *)
