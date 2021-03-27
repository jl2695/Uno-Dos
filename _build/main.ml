open State
open Person
open Command
open Deck

let rec start_turns pos st =
  print_string
    ("It's player "
    ^ string_of_int (pos + 1)
    ^ "'s turn. Place a card or draw: ");
  let people = get_people st in
  let player = people.(pos) in
  let deck = get_curr_deck st in
  match parse (read_line ()) with
  | Draw -> draw player deck
  | Place c -> ()
  | Name n -> ()
  | Begin -> failwith ""

let rec print hand =
  match hand with
  | [] -> print_string "\n"
  | h :: t -> (
      match h.number with
      | Some h ->
          print_int h;
          print t
      | None -> ())

let rec play_game st =
  let people = get_people st in
  for i = 0 to Array.length (get_people st) - 1 do
    let hand = people.(i).hand in
    print_string ((get_people st).(i).name ^ "\n");
    print hand
  done;
  start_turns 0 st

let rec transfer_names name_lst = Array.of_list name_lst

let rec prompt name_lst =
  match parse (read_line ()) with
  | exception End_of_file -> ()
  | Name name ->
      print_string "Enter the next player's name or begin: ";
      prompt (name :: name_lst)
  | Begin ->
      let name_arr = transfer_names (List.rev name_lst) in
      play_game (init_state (Array.length name_arr) name_arr)
  | Draw -> prompt name_lst
  | Place card -> prompt name_lst

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Uno";
  print_string "\nEnter the first player's name: ";
  prompt []

(* USE TRY BLOCKS FOR EXCEPTIONS *)

let () = main ()

(* ocamlbuild -pkgs ANSITerminal main.byte && ./main.byte *)
