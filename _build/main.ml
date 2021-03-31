open State
open Person
open Command
open Deck

let string_of_color_option = function
  | None -> ""
  | Some Red -> "(R)"
  | Some Yellow -> "(Y)"
  | Some Blue -> "(B)"
  | Some Green -> "(G)"

let string_of_color = function
  | Red -> "(R)"
  | Yellow -> "(Y)"
  | Blue -> "(B)"
  | Green -> "(G)"

let rec print hand =
  match hand with
  | [] -> print_string "\n"
  | h :: t -> (
      match h.number with
      | Some n -> (
          print_int n;
          match h.color with
          | Some h ->
              print_string (string_of_color h ^ " ");
              print t
          | None -> ())
      | None -> ())

let string_of_int_option = function
  | None -> ""
  | Some num -> string_of_int num

let rec turns pos st =
  let people = get_people st in
  let deck = get_curr_deck st in
  let pile = get_card_pile st in
  let player = people.(pos) in
  let deck_length = List.length !deck in
  let num_players = Array.length people in
  let next_pos = (pos + 1) mod num_players in
  print_endline
    ("It's " ^ player.name
   ^ "'s turn. Place a card, draw or sort your hand.");
  print_string (player.name ^ "'s hand: ");
  print player.hand;
  print_endline
    ("Pile: "
    ^ string_of_int_option pile.number
    ^ string_of_color_option pile.color);
  print_string "> ";
  match parse (read_line ()) with
  | Draw ->
      print_endline ("Cards left: " ^ string_of_int deck_length);
      turns next_pos (draw_st st pos deck)
  | Place card_index ->
      if int_of_string card_index <= List.length player.hand then (
        let player_card =
          List.nth people.(pos).hand (int_of_string card_index)
        in
        if
          pile.number = player_card.number
          || pile.number = None
          || pile.color = player_card.color
        then turns next_pos (place_st st pos (int_of_string card_index))
        else print_endline "That is an invalid card! Try again.\n";
        turns pos st)
      else
        print_endline "That card index is bigger than your hand size!\n";
      turns pos st
  | Sort -> turns pos (sort_st st pos)
  | Name n -> ()
  | Begin -> failwith ""
  | exception Malformed ->
      print_endline
        "That isn't a valid command! Either place or draw a card.\n";
      turns pos st
  | exception Empty ->
      print_endline
        "That isn't a valid command! Either place or draw a card.\n";
      turns pos st

(* CATCH INDEX OUT OF BOUNDS *)

let rec transfer_names name_lst = Array.of_list name_lst

let rec prompt name_lst =
  print_string "Enter the next player's name or begin: ";
  match parse (read_line ()) with
  | exception End_of_file -> ()
  | Name name -> prompt (name :: name_lst)
  | Begin ->
      let name_arr = transfer_names (List.rev name_lst) in
      if Array.length name_arr > 0 then
        turns 0 (init_state (Array.length name_arr) name_arr)
      else
        print_endline "Enter a player's name first before beginning!\n";
      prompt name_lst
  | Draw ->
      print_endline "Cannot draw a card before the game starts!\n";
      prompt name_lst
  | Place card ->
      print_endline "Cannot place a card before the game starts!\n";
      prompt name_lst
  | Sort ->
      print_endline "Cannot sort a card before the game starts!\n";
      prompt name_lst
  | exception Empty ->
      print_endline "Try again using the format: name player_name\n";
      prompt name_lst
  | exception Malformed ->
      print_endline "Try again using the format: name player_name\n";
      prompt name_lst

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Uno\n";
  prompt []

let () = main ()

(* Run using the following line below *)
(* ocamlbuild -pkgs ANSITerminal main.byte && ./main.byte *)
