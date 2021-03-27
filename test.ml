open OUnit2
open Person
open Command
open Deck
open State

(* let simple_test (name : string) func_output expected_output : test =
   name >:: fun _ -> assert_equal expected_output func_output let
   game_tests = [ simple_test ]

   let suite = "test suite for A2" >::: List.flatten [ game_tests ]

   let _ = run_test_tt_main suite *)

let hand =
  (get_people (init_state 3 [| "James"; "Aki"; "Alden" |])).(0).hand

let rec print () hnd =
  match hnd with
  | [] -> print_string "no more"
  | h :: t -> (
      match h.number with
      | Some h ->
          print_int h;
          print () t
      | None -> print_string "yo mama" )

let print_main () = print () hand

let rec print2 () =
  print_string
    (get_people (init_state 3 [| "James"; "Aki"; "Alden" |])).(0).name;
  print_string
    (get_people (init_state 3 [| "James"; "Aki"; "Alden" |])).(1).name;
  print_string
    (get_people (init_state 3 [| "James"; "Aki"; "Alden" |])).(2).name

(* let print2 () = print_int (get_people (init_state 10)).(0).position *)
