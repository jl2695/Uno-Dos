open State
open Person
open Command
open Deck
open Uno
open Dos

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
              | None -> () )
          | None -> () )
      | Skip -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "Skp";
              print_string " ";
              print t
          | None -> () )
      | Reverse -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "Rev";
              print_string " ";
              print t
          | None -> () )
      | DrawTwo -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "D2 ";
              print_string " ";
              print t
          | None -> () )
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
          print t
      | WildDos ->
          ANSITerminal.print_string
            [ ANSITerminal.on_white; ANSITerminal.black ]
            "Dos";
          print_string " ";
          print t
      | WildNum -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) " # ";
              print_string " ";
              print t
          | None -> () ) )

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
  | WildDos -> print_color (string_of_color_option pile.color) "Dos"
  | WildNum -> print_color (string_of_color_option pile.color) " # "

let idx = ref 0

let rec ai_valid_cards_aux ai_hand acc deck pile dos =
  match ai_hand with
  | [] ->
      idx := 0;
      acc
  | h :: t ->
      idx := !idx + 1;
      if (not dos) && is_valid_card h pile then
        ai_valid_cards_aux t ((!idx - 1) :: acc) deck pile dos
      else if dos && is_valid_dos h pile then
        ai_valid_cards_aux t ((!idx - 1) :: acc) deck pile dos
      else ai_valid_cards_aux t acc deck pile dos

let ai_valid_cards st pos dos =
  let people = get_people st in
  let deck = get_curr_deck st in
  let pile = get_card_pile st in
  let ai = people.(pos) in
  ai_valid_cards_aux ai.hand [] deck pile dos

let compare_custom p1 p2 =
  if p1.score > p2.score then 1
  else if p1.score < p2.score then -1
  else 0

let score_sorted_people st =
  let people = get_people st in
  Array.sort compare_custom people;
  people

let print_reds str =
  ANSITerminal.print_string
    [ ANSITerminal.on_red; ANSITerminal.yellow ]
    str

let print_blues str =
  ANSITerminal.print_string
    [ ANSITerminal.on_cyan; ANSITerminal.yellow ]
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

let rec print_dos_cards hand =
  match hand with
  | [] -> print_string "\n"
  | h :: t ->
      print_blues "DOS";
      print_string " ";
      print_dos_cards t

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

let end_game st pos =
  erase Screen;
  let people = get_people st in
  let player = people.(pos) in
  print_endline_centered
    (player.name ^ " has won this round! Congratulations :))")

(** [print_other_players_hands pos st init_pos] prints the hands of
    every player except for the player at position [init_pos] *)
let rec print_other_players_hands pos st init_pos dos =
  let people = get_people st in
  let player = people.(pos) in
  let num_players = Array.length people in
  let next_pos = (pos + 1) mod num_players in
  let hand_description = player.name ^ "'s hand: \n" in
  if pos <> init_pos then (
    print_centered hand_description;
    center_cursor (String.make (4 * List.length player.hand) ' ');
    if not dos then print_cards player.hand
    else print_dos_cards player.hand;
    print_newline ();
    print_other_players_hands next_pos st init_pos dos )
  else ()

let check_empty_pile st prev_player_pos =
  let pile = get_card_pile st in
  print_centered "The last card placed was ";
  print_pile pile;
  print_newline ()

let game_over () =
  erase Screen;
  center_cursor "GAMEOVER - DECK IS OUT OF CARDS";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "GAMEOVER - DECK IS OUT OF CARDS \n";
  exit 0

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
    print_other_players_hands next_pos st pos false;
    let player_prompt =
      "It's " ^ player.name ^ "'s turn. Place a card or draw a card."
    in
    print_endline_centered player_prompt;
    let hand_description = player.name ^ "'s hand: \n" in
    print_centered hand_description;
    center_cursor (String.make (4 * List.length player.hand) ' ');
    print player.hand;
    (* print_string "\n"; print_endline (print_indices player.hand 0
       ""); *)
    check_empty_pile st prev_pos;
    ANSITerminal.set_cursor ((width / 2) - 8) height;
    print_string "> ";
    match parse (read_line ()) with
    | Draw -> (
        erase ANSITerminal.Screen;
        print_endline_centered
          ("Cards left in the deck: " ^ string_of_int deck_length);
        try turns next_pos (draw_st st pos deck 1)
        with NoMoreCards -> game_over () )
    | Place card_index -> (
        match int_of_string card_index with
        | idx ->
            (* Check to see if the card index is valid in the player's
               hand *)
            if idx <= List.length player.hand - 1 && idx >= 0 then
              let player_card = List.nth player.hand idx in
              (* If the card has the same number or color as the pile or
                 is uncolored, then place that card. *)
              if is_valid_card player_card pile then
                let next_st = place_st st pos idx in
                if get_game_ended next_st then (
                  end_game st pos;
                  if get_curr_round st < get_total_rounds st then (
                    print_endline_centered
                      ( "Press enter to play the next round. Round "
                      ^ string_of_int (get_curr_round st)
                      ^ " over." );
                    cursor_middle ();
                    match read_line () with
                    | _ ->
                        erase Screen;
                        turns pos
                          (reinitialize_state st
                             (get_curr_round st + 1)
                             pos false) )
                  else (
                    erase Screen;
                    let winner =
                      (score_sorted_people st).(Array.length people - 1)
                        .name
                    in
                    let victory_msg =
                      "End of the game. Congratulations " ^ winner
                      ^ ", you had the most wins!"
                    in
                    center_cursor victory_msg;
                    ANSITerminal.print_string [ ANSITerminal.green ]
                      (victory_msg ^ "\n") );
                  exit 0 )
                else (
                  erase Screen;
                  turns (get_pos next_st) next_st
                  (* The card at the card index is invalid and user is
                     prompted again. *) )
              else (
                erase Screen;
                print_endline_centered
                  "That is an invalid card! Try again.\n";
                turns pos st )
            else (
              (* The initial card index input by the user is invalid. *)
              erase Screen;
              print_endline_centered
                "That card index is invalid! (either bigger than your \
                 hand size or less than 0)\n";
              turns pos st )
        | exception Failure s ->
            erase Screen;
            print_endline_centered
              "That isn't a valid command! Either place or draw a card.\n";
            turns pos st )
    (* Covering all match cases *)
    | PlaceDos ->
        print_endline_centered "Invalid command for Uno!";
        turns pos st
    | exception Malformed ->
        erase Screen;
        print_endline_centered
          "That isn't a valid command! Either place or draw a card.\n";
        turns pos st
    | exception Empty ->
        erase Screen;
        print_endline_centered
          "That isn't a valid command! Either place or draw a card.\n";
        turns pos st )
  else
    let valid_cards = ai_valid_cards st pos false in
    if valid_cards = [] then (
      print_endline_centered
        ( player.name ^ " drew from the deck. Cards left: "
        ^ string_of_int deck_length );
      try turns next_pos (draw_st st pos deck 1)
      with NoMoreCards -> game_over () )
    else
      let next_st = place_st st pos (List.hd valid_cards) in

      if get_game_ended next_st then (
        erase Screen;
        end_game st pos;
        ( if get_curr_round st < get_total_rounds st then (
          print_endline_centered
            ( "Press enter to play the next round. Round "
            ^ string_of_int (get_curr_round st)
            ^ " over." );
          cursor_middle ();
          match read_line () with
          | _ ->
              erase Screen;
              turns pos
                (reinitialize_state st
                   (get_curr_round st + 1)
                   pos false) )
        else
          let victory_msg = "End of the game." in
          center_cursor victory_msg;
          ANSITerminal.print_string [ ANSITerminal.green ]
            (victory_msg ^ "\n") );
        exit 0 )
      else turns (get_pos next_st) next_st

let print_dos_piles st =
  let pile = get_card_pile st in
  let snd_pile = get_dos_pile st in
  print_centered "The center piles are: ";
  print_pile pile;
  print_string " ";
  print_pile snd_pile;
  print_newline ()

(** [print_dos hand] Prints player hand [hand] in a form that shows all
    the valid card matching combinations in [hand].*)
let rec print_dos hand =
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
              | None -> () )
          | None -> () )
      | Skip -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "Skp";
              print_string " ";
              print t
          | None -> () )
      | Reverse -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "Rev";
              print_string " ";
              print t
          | None -> () )
      | DrawTwo -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) "D2 ";
              print_string " ";
              print t
          | None -> () )
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
          print t
      | WildDos ->
          ANSITerminal.print_string
            [ ANSITerminal.on_white; ANSITerminal.black ]
            "Dos";
          print_string " ";
          print t
      | WildNum -> (
          match h.color with
          | Some col ->
              print_color (string_of_color col) " # ";
              print_string " ";
              print t
          | None -> () ) )

let remove_opt = function None -> 0 | Some i -> i

let rec find_complement card hand pile c_idx h_idx =
  match hand with
  | [] -> []
  | h :: t ->
      if h.number <> None then
        let c_num = remove_opt card.number in
        let h_num = remove_opt h.number in
        let p_num = remove_opt pile.number in
        if c_num + h_num = p_num then [ (card, c_idx); (h, h_idx) ]
        else find_complement card t pile c_idx (h_idx + 1)
      else find_complement card t pile c_idx (h_idx + 1)

let rec group_hand hand org_hand pile acc idx =
  match hand with
  | [] -> acc
  | h :: t ->
      let comp = find_complement h t pile idx (idx + 1) in
      if comp <> [] then
        let last_idx = snd (List.hd comp) in
        let comp_ele = List.nth org_hand last_idx in
        let remove_comp_ele = List.filter (fun x -> x <> comp_ele) t in
        group_hand remove_comp_ele org_hand pile (comp :: acc) (idx + 1)
      else group_hand t org_hand pile acc (idx + 1)

let rec match_pile () =
  match parse_pile (read_line ()) with
  | 1 -> 1
  | 2 -> 2
  | exception Malformed ->
      print_centered "Please enter 1 or 2.";
      match_pile ()
  | exception Empty ->
      print_centered "Please enter 1 or 2.";
      match_pile ()
  | _ ->
      print_centered "Please enter 1 or 2.";
      match_pile ()

let print_card card =
  match card.ctype with
  | Normal -> (
      match card.number with
      | Some n -> (
          match card.color with
          | Some col ->
              print_color (string_of_color col)
                (" " ^ string_of_int n ^ " ");
              print_string " "
          | None -> () )
      | None -> () )
  | WildDos ->
      ANSITerminal.print_string
        [ ANSITerminal.on_white; ANSITerminal.black ]
        "Dos";
      print_string " "
  | WildNum -> (
      match card.color with
      | Some col ->
          print_color (string_of_color col) " # ";
          print_string " "
      | None -> () )
  | _ -> print_string " "

let print_group grp idx =
  let first = fst (List.hd grp) in
  let second = fst (List.hd (List.rev grp)) in
  print_string (string_of_int idx ^ ": ");
  print_string "( ";
  print_card first;
  print_card second;
  print_string ")"

(** [print_groups grps] prints out each element of a group of two
    obtained from the list of groups from group_hand *)
let rec print_groups grps h_length =
  match grps with
  | [] -> ()
  | h :: t ->
      print_group h h_length;
      print_groups t (h_length + 1);
      print_string "\n"

let rec dos_turns pos st =
  let people = get_people st in
  let deck = get_curr_deck st in
  let player = people.(pos) in
  let deck_length = List.length !deck in
  let num_players = Array.length people in
  let next_pos = (pos + 1) mod num_players in
  if not player.ai then (
    print_other_players_hands next_pos st pos true;
    print_dos_piles st;
    print_endline_centered (player.name ^ "'s hand:");
    center_cursor (String.make (4 * List.length player.hand) ' ');
    print player.hand;
    let player_prompt =
      "It's " ^ player.name
      ^ "'s turn. Place a card(s) or draw a card. If you want to place \
         a card, type place."
    in
    print_endline_centered player_prompt;
    ANSITerminal.set_cursor ((width / 2) - 8) height;
    print_string "> ";
    match parse (read_line ()) with
    | Draw -> (
        erase ANSITerminal.Screen;
        print_endline_centered
          ("Cards left in the deck: " ^ string_of_int deck_length);
        try dos_turns next_pos (draw_st st pos deck 1)
        with NoMoreCards -> game_over () )
    | Place _ ->
        print_endline_centered
          "Invalid command for Dos. Please just type place if you \
           would like to place card(s).";
        dos_turns pos st
    | PlaceDos -> (
        print_centered
          "Which pile would you like to match with? (1 or 2) ";
        let mch_pile = match_pile () in
        erase Screen;
        ANSITerminal.set_cursor ((width / 2) - 8) height;
        print_string "> ";
        let pile =
          if mch_pile = 1 then get_card_pile st else get_dos_pile st
        in
        let grps = group_hand player.hand player.hand pile [] 0 in
        let hand_description = player.name ^ "'s hand: \n" in
        print_centered hand_description;
        center_cursor (String.make (4 * List.length player.hand) ' ');
        print player.hand;
        print_endline_centered
          "In brackets are the possible pairs of cards you can put \
           down. (If there are no brackets, there are no possible \
           pairs)";
        ANSITerminal.set_cursor ((width / 2) - 8) height;
        print_groups grps (List.length player.hand);
        print_dos_piles st;
        print_centered "Place card(s). (type 'place card_index') \n";
        ANSITerminal.set_cursor ((width / 2) - 8) height;
        print_string "> ";
        match parse (read_line ()) with
        | Place card_index -> (
            match int_of_string card_index with
            | idx ->
                (* Check to see if the card index is valid in the
                   player's hand *)
                if idx <= List.length player.hand - 1 && idx >= 0 then
                  let player_card = List.nth player.hand idx in
                  (* If the card has the same number or color as the
                     pile or is uncolored, then place that card. *)
                  if is_valid_dos player_card pile then
                    let next_st =
                      place_st_dos_single st pos idx mch_pile
                    in
                    if get_game_ended next_st then (
                      end_game st pos;
                      if get_curr_round st < get_total_rounds st then (
                        print_endline_centered
                          ( "Press enter to play the next round. Round "
                          ^ string_of_int (get_curr_round st)
                          ^ " over." );
                        cursor_middle ();
                        match read_line () with
                        | _ ->
                            erase Screen;
                            dos_turns pos
                              (reinitialize_state st
                                 (get_curr_round st + 1)
                                 pos true) )
                      else (
                        erase Screen;
                        let winner =
                          (score_sorted_people st).(Array.length people
                                                    - 1)
                            .name
                        in
                        let victory_msg =
                          "End of the game. Congratulations " ^ winner
                        in
                        center_cursor victory_msg;
                        ANSITerminal.print_string [ ANSITerminal.green ]
                          (victory_msg ^ "\n") );
                      exit 0 )
                    else (
                      erase Screen;
                      dos_turns (get_pos next_st) next_st )
                    (* The card at the card index is invalid and user is
                       prompted again. *)
                  else invalid_command pos st
                else
                  (****** START DOUBLE MATCH ********)
                  let hand_length = List.length player.hand - 1 in
                  if
                    idx > hand_length
                    && idx <= hand_length + List.length grps
                  then
                    let grp_idx = idx - List.length player.hand in
                    let next_st =
                      place_st_dos_double st pos grp_idx grps mch_pile
                    in

                    if get_game_ended next_st then (
                      end_game st pos;
                      if get_curr_round st < get_total_rounds st then (
                        print_endline_centered
                          ( "Press enter to play the next round. Round "
                          ^ string_of_int (get_curr_round st)
                          ^ " over." );
                        cursor_middle ();
                        match read_line () with
                        | _ ->
                            erase Screen;
                            dos_turns pos
                              (reinitialize_state st
                                 (get_curr_round st + 1)
                                 pos true) )
                      else (
                        erase Screen;
                        let winner =
                          (score_sorted_people st).(Array.length people
                                                    - 1)
                            .name
                        in
                        let victory_msg =
                          "End of the game. Congratulations " ^ winner
                        in
                        center_cursor victory_msg;
                        ANSITerminal.print_string [ ANSITerminal.green ]
                          (victory_msg ^ "\n") );
                      exit 0 )
                    else dos_turns pos st
                  else (
                    (* The initial card index input by the user is
                       invalid. *)
                    erase Screen;
                    print_endline_centered
                      "That card index is invalid! (either bigger than \
                       your hand size or less than 0)\n";
                    dos_turns pos st )
            | exception Failure _ -> invalid_command pos st )
        | exception _ -> invalid_command pos st
        | _ -> invalid_command pos st )
    (* Covering all match cases *)
    | exception Malformed -> invalid_command pos st
    | exception Empty -> invalid_command pos st )
  else
    let valid_cards = ai_valid_cards st pos true in
    if valid_cards = [] then (
      print_endline_centered
        ( player.name ^ " drew from the deck. Cards left: "
        ^ string_of_int deck_length );
      try dos_turns next_pos (draw_st st pos deck 1)
      with NoMoreCards -> game_over () )
    else
      let next_st = place_st st pos (List.hd valid_cards) in
      if get_game_ended next_st then (
        erase Screen;
        end_game st pos;
        ( if get_curr_round st < get_total_rounds st then (
          print_endline_centered
            ( "Press enter to play the next round. Round "
            ^ string_of_int (get_curr_round st)
            ^ " over." );
          cursor_middle ();
          match read_line () with
          | _ ->
              erase Screen;
              dos_turns pos
                (reinitialize_state st (get_curr_round st + 1) pos true)
          )
        else
          let victory_msg =
            "End of the game. Tough luck, we'll get em next time."
          in
          center_cursor victory_msg;
          ANSITerminal.print_string [ ANSITerminal.green ]
            (victory_msg ^ "\n") );
        exit 0 )
      else dos_turns (get_pos next_st) next_st

and invalid_command pos st =
  erase Screen;
  print_endline_centered
    "That isn't a valid command! Either place or draw a card.\n";
  dos_turns pos st

let ai_names =
  [
    "John (AI)";
    "Jane (AI)";
    "Bob (AI)";
    "Rick (AI)";
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
    then places the player's name into [name_lst]. *)
let rec prompt name_lst ai_name_lst rounds beg1 beg2 beg3 =
  if beg3 then (
    let name_arr = transfer_names (List.rev name_lst) in
    let ai_name_arr = transfer_names ai_name_lst in
    print_endline "Would you like to play uno or dos?";
    match read_line () with
    | exception End_of_file -> ()
    | "uno" ->
        turns 0
          ( try
              init_state (Array.length name_arr) name_arr
                (Array.length ai_name_arr)
                ai_name_arr rounds false
            with NoMoreCards -> game_over () )
    | "dos" ->
        dos_turns 0
          ( try
              init_state (Array.length name_arr) name_arr
                (Array.length ai_name_arr)
                ai_name_arr rounds true
            with NoMoreCards -> game_over () )
    | _ ->
        print_endline "Please enter either uno or dos.";
        prompt name_lst ai_name_lst rounds beg1 beg2 beg3 )
  else if beg2 then (
    print_endline
      "Enter the number of rounds you would like to play (1, 3, or 5).";
    match int_of_string (read_line ()) with
    | x when x = 1 || x = 3 || x = 5 ->
        prompt name_lst ai_name_lst x true true true
    | _ ->
        print_endline "Please choose a number from 1, 3, or 5. ";
        prompt name_lst ai_name_lst rounds beg1 beg2 beg3
    | exception Failure s ->
        print_endline "Please enter a number. ";
        prompt name_lst ai_name_lst rounds beg1 beg2 beg3 )
  else if beg1 then (
    print_endline
      "Enter the number of AIs you want to play with. (Maximum is 7)";
    match int_of_string (read_line ()) with
    | ai_num ->
        prompt name_lst
          (sublist ai_names [] ai_num)
          rounds true true false
    | exception Failure s ->
        print_endline "Please enter a number.";
        prompt name_lst ai_name_lst rounds true false false )
  else
    print_endline
      "Enter the next player's name or press enter to continue.";
  match read_line () with
  | exception End_of_file -> ()
  | "" ->
      let name_arr = transfer_names (List.rev name_lst) in
      if Array.length name_arr = 0 then (
        print_endline "Enter a player's name first before beginning!\n";
        prompt name_lst ai_name_lst rounds false beg2 beg3 )
      else prompt name_lst ai_name_lst rounds true beg2 beg3
  | name -> prompt (name :: name_lst) ai_name_lst rounds false beg2 beg3

(** [main ()] begins the game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nWelcome to Uno-Dos\n";
  prompt [] [] 0 false false false

(* Executes the game engine. *)
let () = main ()
