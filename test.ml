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

let print () = print_string (get_people (init_state 5)).(0).name
