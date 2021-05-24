(** Test plan: Our test suite tests the initial deck and state for both
    the Uno and Dos game. We made helper functions to test properties of
    the deck - for example, the helper functions counts how many cards
    there are in a deck for a specific color or type. We also tested the
    parsing commands that are used to set up the game and are also used
    during gameplay. Thus, the modules that were tested by OUnit are
    command.ml, person.ml, state.ml, and deck.ml. These test cases were
    developed through a mix of black box and glass box testing so that
    we test from the point of view as a programmer/developer and from
    the point of view as a client/player - we thought we could identify
    any errors hidden in our code most effectively this way. The other
    module, namely the functions in main.ml were playtested. By testing
    in this manner, we were able to ensure the correctness for this
    system since for the smaller scale functions (e.g. initiliazing a
    deck or drawing a card) we used OUnit and were able to pinpoint
    errors in the code easily, while for larger scale functions (those
    in main.ml) we thoroughly playtested and could spot errors within
    the context of the gameplay. *)

open OUnit2
open Person
open Command
open Deck
open State

let simple_test (name : string) func_output expected_output : test =
  name >:: fun _ -> assert_equal expected_output func_output

let simple_test_int (name : string) func_output expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output func_output ~printer:string_of_int

let exc_test (name : string) func_output expected_output : test =
  name >:: fun _ -> assert_raises expected_output func_output

let rec color_num_check_aux color deck acc =
  match deck with
  | [] -> acc
  | h :: t ->
      if h.color = color then color_num_check_aux color t (acc + 1)
      else color_num_check_aux color t acc

let color_number_checker color deck = color_num_check_aux color deck 0

let rec type_num_check_aux typ deck acc =
  match deck with
  | [] -> acc
  | h :: t ->
      if h.ctype = typ then type_num_check_aux typ t (acc + 1)
      else type_num_check_aux typ t acc

let type_number_checker typ deck = type_num_check_aux typ deck 0

let rec num_check_aux num deck acc =
  match deck with
  | [] -> acc
  | h :: t ->
      if h.number = num then num_check_aux num t (acc + 1)
      else num_check_aux num t acc

let num_checker num deck = num_check_aux num deck 0

let bas_init_st =
  init_state 3 [| "James"; "Aki"; "Alden" |] 0 [||] 3 false

let bas_init_st' =
  init_state 4 [| "James"; "Aki"; "Alden"; "Kevin" |] 0 [||] 3 false

let draw_one_aki_st = draw_st bas_init_st' 1 bas_init_st'.curr_deck 1

(* similar copy of above state - needed to test draw because of the
   mutable nature of state *)

let bas_init_st'' =
  init_state 3 [| "James"; "Aki"; "Alden" |] 1 [| "BOB" |] 3 false

let draw_two_alden_st =
  draw_st bas_init_st'' 2 bas_init_st''.curr_deck 2

let init_tests_uno =
  [
    simple_test_int "init of deck" (List.length !(Deck.init ())) 112;
    simple_test_int "init of deck # of red cards"
      (color_number_checker (Some Red) !(Deck.init ()))
      26;
    simple_test_int "init of deck # of blue cards"
      (color_number_checker (Some Blue) !(Deck.init ()))
      26;
    simple_test_int "init of deck # of green cards"
      (color_number_checker (Some Green) !(Deck.init ()))
      26;
    simple_test_int "init of deck # of yellow cards"
      (color_number_checker (Some Yellow) !(Deck.init ()))
      26;
    simple_test_int "init of deck # of 5s"
      (num_checker (Some 5) !(Deck.init ()))
      8;
    simple_test_int "init of deck # of 0s"
      (num_checker (Some 0) !(Deck.init ()))
      8;
    simple_test_int "init of deck # of 10s"
      (num_checker (Some 10) !(Deck.init ()))
      0;
    simple_test_int "init of deck # of normal cards"
      (type_number_checker Normal !(Deck.init ()))
      80;
    simple_test_int "init of deck # of skip cards"
      (type_number_checker Skip !(Deck.init ()))
      8;
    simple_test_int "init of deck # of reverse cards"
      (type_number_checker Reverse !(Deck.init ()))
      8;
    simple_test_int "init of deck # of DrawTwo cards"
      (type_number_checker DrawTwo !(Deck.init ()))
      8;
    simple_test_int "init of deck # of DrawFour cards"
      (type_number_checker DrawFour !(Deck.init ()))
      4;
    simple_test_int "init of deck # of Wild cards"
      (type_number_checker Wild !(Deck.init ()))
      4;
    simple_test_int "initial state fields people"
      (Array.length bas_init_st.people)
      3;
    simple_test "initial state fields people - name"
      bas_init_st.people.(1).name "Aki";
    simple_test_int "initial state fields people - hand size"
      (List.length bas_init_st.people.(0).hand)
      7;
    simple_test_int "initial state fields people - position"
      bas_init_st.people.(0).position 1;
    simple_test_int "initial state fields people - score"
      bas_init_st.people.(0).score 0;
    simple_test "initial state fields people - ai"
      bas_init_st.people.(0).ai false;
    simple_test_int "initial state fields curr_deck"
      (List.length !(bas_init_st.curr_deck))
      90;
    simple_test_int "initial state fields pos" bas_init_st.pos 0;
    simple_test "initial state fields game_ended" bas_init_st.game_ended
      false;
    simple_test_int "initial state fields curr_round"
      bas_init_st.curr_round 1;
    simple_test_int "initial state fields total_rounds"
      bas_init_st.total_rounds 3;
  ]

let parse_tests =
  [
    simple_test "parse place" (parse " place   0") (Place "0");
    simple_test "parse draw" (parse " draw") Draw;
    simple_test "parse palce for dos" (parse "place") PlaceDos;
    exc_test "parse invalid command"
      (fun () -> parse " draw 0")
      Malformed;
    simple_test "parse colors - red" (parse_colors "  red") Red;
    simple_test "parse colors - blue" (parse_colors "blue  ") Blue;
    simple_test "parse colors - yellow" (parse_colors "yellow") Yellow;
    simple_test "parse colors - green" (parse_colors "green") Green;
  ]

let draw_tests =
  [
    simple_test_int "draw one aki"
      (List.length draw_one_aki_st.people.(1).hand)
      8;
    simple_test_int "deck size after drawing once"
      (List.length !(draw_one_aki_st.curr_deck))
      82;
    simple_test_int "draw two alden"
      (List.length draw_two_alden_st.people.(2).hand)
      9;
    simple_test_int "deck size after drawing twice"
      (List.length !(draw_two_alden_st.curr_deck))
      81;
  ]

let init_tests_dos =
  [
    simple_test_int "init of deck" (List.length !(Deck.init_dos ())) 108;
    simple_test_int "init of deck # of normal cards"
      (type_number_checker Normal !(Deck.init_dos ()))
      88;
    simple_test_int "init of deck # of WildNum cards"
      (type_number_checker WildNum !(Deck.init_dos ()))
      8;
    simple_test_int "init of deck # of WildDos cards"
      (type_number_checker WildDos !(Deck.init_dos ()))
      12;
    simple_test_int "number of zeros"
      (num_checker (Some 0) !(Deck.init_dos ()))
      0;
    simple_test_int "number of twos"
      (num_checker (Some 2) !(Deck.init_dos ()))
      0;
    simple_test_int "number of ones"
      (num_checker (Some 1) !(Deck.init_dos ()))
      12;
    simple_test_int "number of tens"
      (num_checker (Some 10) !(Deck.init_dos ()))
      8;
    simple_test_int "number of elevens"
      (num_checker (Some 11) !(Deck.init_dos ()))
      0;
    simple_test_int "number of blues"
      (color_number_checker (Some Blue) !(Deck.init_dos ()))
      24;
    simple_test_int "number of greens"
      (color_number_checker (Some Green) !(Deck.init_dos ()))
      24;
    simple_test_int "number of reds"
      (color_number_checker (Some Red) !(Deck.init_dos ()))
      24;
    simple_test_int "number of yellows"
      (color_number_checker (Some Yellow) !(Deck.init_dos ()))
      24;
  ]

let suite =
  "test suite for Uno-Dos"
  >::: List.flatten
         [ init_tests_uno; parse_tests; init_tests_dos; draw_tests ]

let _ = run_test_tt_main suite
