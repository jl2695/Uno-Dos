open State
open Person
open Command
open Deck

let string_of_option = function
  | None -> ""
  | Some num -> string_of_int num

let rec turns pos st =
  let people = get_people st in
  let deck = get_curr_deck st in
  let pile = get_card_pile st in
  let deck_length = List.length !deck in
  let num_players = Array.length people in
  let next_pos = (pos + 1) mod num_players in
  print_string
    ("It's player "
    ^ string_of_int (pos + 1)
    ^ "'s turn. Place a card or draw: \n");
  print_endline ("Pile: " ^ string_of_option pile.number);
  print_string "> ";
  match parse (read_line ()) with
  | Draw ->
      print_endline ("Cards left: " ^ string_of_int deck_length);
      turns next_pos (draw_st st pos deck)
  | Place card_index -> turns next_pos (place_st st pos)
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
  turns 0 st

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

(* Run using the following line below *)
(* ocamlbuild -pkgs ANSITerminal main.byte && ./main.byte *)
