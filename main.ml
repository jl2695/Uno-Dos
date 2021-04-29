open State
open Person
open Command
open Deck

(** [string_of_color_option color_opt] Returns a string of a color
    option [color_opt].*)
let string_of_color_option = function
  | None -> [ ANSITerminal.red ]
  | Some Red -> [ ANSITerminal.red ]
  | Some Yellow -> [ ANSITerminal.yellow ]
  | Some Blue -> [ ANSITerminal.blue ]
  | Some Green -> [ ANSITerminal.green ]

(** [string_of_color color] Returns a string of a color [color].*)
let string_of_color = function
  | Red -> [ ANSITerminal.red ]
  | Yellow -> [ ANSITerminal.yellow ]
  | Blue -> [ ANSITerminal.blue ]
  | Green -> [ ANSITerminal.green ]

let print_color color str =
  ANSITerminal.print_string (ANSITerminal.on_white :: color) str

(** [print hand] Prints player hand [hand].*)
let rec print hand =
  match hand with
  | [] -> print_string "\n"
  | h :: t -> (
      match h.ctype with
      | Normal -> (
          match h.number with
          | Some n -> (
              match h.color with
              | Some col ->
                  print_color (string_of_color col)
                    (" " ^ string_of_int n ^ " ");
                  print_string " ";
                  print t
              | None -> ())
          | None -> ())
      | Skip -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "Skp";
              print_string " ";
              print t
          | None -> ())
      | Reverse -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "Rev";
              print_string " ";
              print t
          | None -> ())
      | DrawTwo -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "D2 ";
              print_string " ";
              print t
          | None -> ())
      | DrawFour ->
          ANSITerminal.print_string
            [ ANSITerminal.on_white; ANSITerminal.black ]
            "D4 ";
          print_string " ";
          print t
      | Wild ->
          ANSITerminal.print_string
            [ ANSITerminal.on_white; ANSITerminal.black ]
            "Wld";
          print_string " ";
          print t)

(** [string_of_int_option opt] Returns a string of an int option [opt]. *)
let string_of_int_option = function
  | None -> ""
  | Some num -> string_of_int num

let print_pile pile =
  match pile.ctype with
  | Normal ->
      if pile.color = None && pile.number = None then ()
      else
        print_color
          (string_of_color_option pile.color)
          (" " ^ string_of_int_option pile.number ^ " ")
  | Skip -> print_color (string_of_color_option pile.color) "Skp"
  | Reverse -> print_color (string_of_color_option pile.color) "Rev"
  | DrawTwo -> print_color (string_of_color_option pile.color) "D2 "
  | DrawFour -> print_color (string_of_color_option pile.color) "D4 "
  | Wild -> print_color (string_of_color_option pile.color) "Wld"

let is_valid_card card deck pile =
  (pile.number != None && pile.number = card.number)
  || (pile.color != None && pile.color = card.color)
  || (pile.number = None && pile.color = None)
  || (card.number = None && card.color = None)
  || (pile.ctype = Reverse && card.ctype = Reverse)
  || (pile.ctype = Skip && card.ctype = Skip)
  || (pile.ctype = DrawTwo && card.ctype = DrawTwo)

let idx = ref 0

let rec ai_valid_cards_aux ai_hand acc deck pile =
  match ai_hand with
  | [] ->
      idx := 0;
      acc
  | h :: t ->
      idx := !idx + 1;
      if is_valid_card h deck pile then
        ai_valid_cards_aux t ((!idx - 1) :: acc) deck pile
      else ai_valid_cards_aux t acc deck pile

let ai_valid_cards st pos =
  let people = get_people st in
  let deck = get_curr_deck st in
  let pile = get_card_pile st in
  let ai = people.(pos) in
  ai_valid_cards_aux ai.hand [] deck pile

let end_game st pos =
  let people = get_people st in
  let player = people.(pos) in
  print_endline (player.name ^ " has won! Congratulations :)))")

let print_reds str =
  ANSITerminal.print_string
    [ ANSITerminal.on_red; ANSITerminal.yellow ]
    str

(** [print_cards hand] prints the cards in [hand] with a red outline and
    no letters or numbers. *)
let rec print_cards hand =
  match hand with
  | [] -> print_string "\n"
  | h :: t ->
      print_reds "UNO";
      print_string " ";
      print_cards t

let size = ANSITerminal.size ()

let width = fst size

let height = snd size

let erase screen = ANSITerminal.erase screen

let cursor_middle () = ANSITerminal.set_cursor (width / 2) height

let center_cursor str =
  let len = String.length str in
  let center = width / 2 in
  ANSITerminal.set_cursor (center - (len / 2)) height

let print_centered str =
  center_cursor str;
  print_string str

let print_endline_centered str =
  center_cursor str;
  print_endline str

let empty_pile = { number = None; color = None; ctype = Normal }

(** [print_other_players_hands pos st init_pos] prints the hands of
    every player except for the player at position [init_pos] *)
let rec print_other_players_hands pos st init_pos =
  let people = get_people st in
  let player = people.(pos) in
  let num_players = Array.length people in
  let next_pos = (pos + 1) mod num_players in
  let hand_description = player.name ^ "'s hand: \n" in
  if pos <> init_pos then (
    print_centered hand_description;
    center_cursor (String.make (4 * List.length player.hand) ' ');
    print_cards player.hand;
    print_other_players_hands next_pos st init_pos)
  else ()

let check_empty_pile st prev_player_pos =
  let pile = get_card_pile st in
  let prev_player = (get_people st).(prev_player_pos) in
  if pile = empty_pile then (
    center_cursor "Pile:     ";
    print_string "Pile: ";
    print_newline ())
  else if pile.ctype = Skip then (
    let num_players = Array.length (get_people st) in
    let prev_prev_player_pos =
      if prev_player_pos - 1 < 0 then num_players - 1
      else (prev_player_pos - 1) mod num_players
    in
    let prev_prev_player = (get_people st).(prev_prev_player_pos) in
    print_centered ("Last turn " ^ prev_prev_player.name ^ " placed ");
    print_pile pile;
    print_newline ())
  else (
    print_centered ("Last turn " ^ prev_player.name ^ " placed ");
    print_pile pile;
    print_newline ())

(** [turns pos st] operates the turns of the game by prompting the
    player in position [pos] to perform an action either "draw", "place
    card_index", or "sort" their hand. When one of these actions is
    performed, the game state [st] changes. Place requires that a string
    for a card index is entered. *)
let rec turns pos st =
  let people = get_people st in
  let deck = get_curr_deck st in
  let pile = get_card_pile st in
  let player = people.(pos) in
  let deck_length = List.length !deck in
  let num_players = Array.length people in
  let next_pos = (pos + 1) mod num_players in
  let prev_pos =
    if pos - 1 >= 0 then (pos - 1) mod num_players else num_players - 1
  in
  if not player.ai then (
    print_other_players_hands next_pos st pos;
    let player_prompt =
      "It's " ^ player.name
      ^ "'s turn. Place a card, draw or sort your hand."
    in
    print_endline_centered player_prompt;
    let hand_description = player.name ^ "'s hand: \n" in
    print_centered hand_description;
    center_cursor (String.make (4 * List.length player.hand) ' ');
    print player.hand;
    (* print_string "\n"; print_endline (print_indices player.hand 0
       ""); *)
    check_empty_pile st prev_pos;
    ANSITerminal.set_cursor ((width / 2) - 10) height;
    print_string "> ";
    match parse (read_line ()) with
    | Draw ->
        erase ANSITerminal.Screen;
        print_endline_centered
          ("Cards left in the deck: " ^ string_of_int deck_length);
        turns next_pos (draw_st st pos deck 1)
    | Place card_index -> (
        match int_of_string card_index with
        | idx ->
            (* Check to see if the card index is valid in the player's
               hand *)
            if
              int_of_string card_index <= List.length player.hand - 1
              && int_of_string card_index >= 0
            then
              let player_card =
                List.nth player.hand (int_of_string card_index)
              in
              (* If the card has the same number or color as the pile or
                 is uncolored, then place that card. *)
              if is_valid_card player_card deck pile then (
                let next_st =
                  place_st st pos (int_of_string card_index)
                in
                if get_game_ended next_st then end_game st pos
                else erase Screen;
                turns (get_pos next_st) next_st
                (* The card at the card index is invalid and user is
                   prompted again. *))
              else (
                erase Screen;
                print_endline_centered
                  "That is an invalid card! Try again.\n";
                turns pos st)
            else (
              (* The initial card index input by the user is invalid. *)
              erase Screen;
              print_endline_centered
                "That card index is invalid! (either bigger than your \
                 hand size or less than 0)\n";
              turns pos st)
        | exception Failure s ->
            erase Screen;
            print_endline_centered
              "That isn't a valid command! Either place or draw a card.\n";
            turns pos st)
    | Sort -> turns pos (sort_st st pos)
    (* Covering all match cases *)
    | AI n -> turns pos st
    | Name n -> turns pos st
    | Begin -> turns pos st
    | exception Malformed ->
        erase Screen;
        print_endline_centered
          "That isn't a valid command! Either place or draw a card.\n";
        turns pos st
    | exception Empty ->
        erase Screen;
        print_endline_centered
          "That isn't a valid command! Either place or draw a card.\n";
        turns pos st)
  else
    let valid_cards = ai_valid_cards st pos in
    if valid_cards = [] then turns next_pos (draw_st st pos deck 1)
    else
      let next_st = place_st st pos (List.hd valid_cards) in
      turns (get_pos next_st) next_st

let ai_names =
  [
    "Aki (AI)";
    "James (AI)";
    "Alden (AI)";
    "Bob (AI)";
    "Frank (AI)";
    "Jenkins (AI)";
    "Lizzy (AI)";
  ]

let rec transfer_names name_lst = Array.of_list name_lst

let rec sublist lst acc n =
  match lst with
  | [] -> acc
  | h :: t -> if n > 0 then sublist t (h :: acc) (n - 1) else acc

(** [prompt name_lst] prompts the user to input the names of each player
    then places the player's name into [name_lst]. If the user enters
    "begin" then the game begins.*)
let rec prompt name_lst ai_name_lst =
  print_string
    "Enter the next player's name or begin. Use the format name \
     player_name for \n\
     entering a player's name and AI number_of_ai for adding AIs: ";
  match parse (read_line ()) with
  | exception End_of_file -> ()
  | Name name -> prompt (name :: name_lst) ai_name_lst
  | AI ai_num -> prompt name_lst (sublist ai_names [] ai_num)
  | Begin ->
      let name_arr = transfer_names (List.rev name_lst) in
      let ai_name_arr = transfer_names ai_name_lst in
      if Array.length name_arr > 0 then
        turns 0
          (init_state (Array.length name_arr) name_arr
             (Array.length ai_name_arr)
             ai_name_arr)
      else (
        print_endline "Enter a player's name first before beginning!\n";
        prompt name_lst ai_name_lst)
  | Draw ->
      print_endline "Cannot draw a card before the game starts!\n";
      prompt name_lst ai_name_lst
  | Place card ->
      print_endline "Cannot place a card before the game starts!\n";
      prompt name_lst ai_name_lst
  | Sort ->
      print_endline "Cannot sort a card before the game starts!\n";
      prompt name_lst ai_name_lst
  | exception Empty ->
      print_endline
        "Try again using the format: name player_name, and AI \
         number_of_ai\n";
      prompt name_lst ai_name_lst
  | exception Malformed ->
      print_endline
        "Try again using the format: name player_name, and AI \
         number_of_ai\n";
      prompt name_lst ai_name_lst

(** [main ()] begins the game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\nWelcome to Uno\n";
  prompt [] []

(* Executes the game engine. *)
let () = main ()
