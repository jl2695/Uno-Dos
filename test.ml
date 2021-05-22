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

let bas_init_st = init_state 3 [| "James"; "Aki"; "Alden" |] 0 [||] 1

let init_tests =
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
    simple_test "initial state fields people"
      bas_init_st.people.(1).name "Aki";
    simple_test_int "initial state fields curr_deck"
      (List.length !(bas_init_st.curr_deck))
      91;
    simple_test "initial state fields pile" bas_init_st.card_pile.number
      None;
    simple_test "initial state fields pile" bas_init_st.card_pile.color
      None;
    simple_test "initial state fields pile" bas_init_st.card_pile.ctype
      Normal;
    simple_test_int "initial state fields pos" bas_init_st.pos 0;
    simple_test "initial state fields game_ended" bas_init_st.game_ended
      false;
    simple_test_int "initial state fields curr_round"
      bas_init_st.curr_round 1;
    simple_test_int "initial state fields total_rounds"
      bas_init_st.total_rounds 1;
  ]

let parse_tests =
  [
    simple_test "parse place" (parse " place   0") (Place "0");
    simple_test "parse draw" (parse " draw") Draw;
    exc_test "parse invalid command"
      (fun () -> parse " draw 0")
      Malformed;
  ]

let suite =
  "test suite for Uno-Dos" >::: List.flatten [ init_tests; parse_tests ]

let _ = run_test_tt_main suite

let rec print () hnd =
  match hnd with
  | [] -> print_string "no more"
  | h :: t -> (
      match h.number with
      | Some h ->
          print_int h;
          print () t
      | None -> print_string "yo mama" )

(* let print_main () = print () (get_people (init_state 3 [| "James";
   "Aki"; "Alden" |])).(1).hand

   let rec print2 () = print_string (get_people (init_state 3 [|
   "James"; "Aki"; "Alden" |])).(0).name; print_string (get_people
   (init_state 3 [| "James"; "Aki"; "Alden" |])).(1).name; print_string
   (get_people (init_state 3 [| "James"; "Aki"; "Alden" |])).(2).name *)

(* let print2 () = print_int (get_people (init_state 10)).(0).position *)
