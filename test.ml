(** 
   Test Suite for the Entire System

   We mainly tested the Grid and State modules to ensure that the 
   implementations of the modules are correct and behave as we expect it to. We 
   also tested the NPC modules to see that they make the decisions we expect 
   them to make given a particular grid/state. For these modules, we try to 
   thoroughly test every function specified in the respective signatures, on 
   all their respective paths through the specification. 

   It is sometimes difficult to test these functions thoroughly without 
   violating the abstraction principle, so many functions are tested in 
   conjunction with other functions in various combinations to ensure that they 
   interact with each other as they should. For example, identifying the cell 
   or box in at a particular position in the grid then getting its position 
   should get the same position and vice versa. 

   For some functions, such as merging and checking for defeat conditions, 
   there are many conditions to be checked. For this, a combination of 
   glass-box and black-box testing is used to thoroughly test the many different 
   possible kinds of input. 

   The tests for the Interface and Savefile functions are omitted because 
   these can be easily verified by interactively running the game, and seeing 
   that the game interface interacts with the user as expected. That said, this 
   test suite is not a substitute for interactively playing the game, nor is 
   the latter a substitute for the former. Hand in hand, both assure us with 
   high confidence that our system and its functions behave correctly.

   With regards to account system, we tested the functionality by first manually
   creating an empty json file called sample_acc. In reality, this account
   should never be created because all the fields are empty, but we did so 
   anyways to make sure that the function does as it intends to even in the 
   lower bound case of empty inputs. A more realistic test case is followed 
   using the json file sample2_acc. This file is created by actually playing the 
   game and saving the account sample2. This account already has values and so
   we tested on those values to make sure that it is correct as intended. This
   include looking at the highest score achieved by the player of that account
   which involves changing the string representation of scores into an
   int list representation and getting the max value out of that int list.
*)

open OUnit2
open Grid
open State
open Npc
open Savefile
open Account

let grid1 = empty ()
let grid2 () = gen_box 2 0 0 (empty ())
let grid2_t = grid2 ()
let gridw = gen_box 2048 2 3 (grid2 ())
let gridl () = empty () |> gen_box 2 0 0 |> gen_box 4 0 1 |> gen_box 8 0 2 |>
               gen_box 16 0 3 |> gen_box 32 1 0 |> gen_box 64 1 1 |>
               gen_box 128 1 2 |> gen_box 256 1 3 |> gen_box 512 2 0 |>
               gen_box 1024 2 1 |> gen_box 2 2 2 
               |> gen_box 8 2 3 |> gen_box 4 3 0 |> gen_box 8 3 1
               |> gen_box 16 3 2 |> gen_box 32 3 3
let gridl_t = gridl ()
let grid3 () = gridl () |> remove_box 2 2
let grid3_t = grid3 ()
let grid4 = gridl () |> remove_box 2 3
let grid5 = gridl () |> remove_box 3 2
let grid6 = grid3 () |> gen_box 1024 2 2
let grid7 = grid3 () |> gen_box 128 2 2
let grid8 = gridl () |> remove_box 1 3 |> gen_box 128 1 3
let grid9 = gridl () |> remove_box 3 2 |> gen_box 8 3 2
let grid10 = gridl () |> remove_box 3 1 |> gen_box 1024 3 1
let grid11 = gridl () |> remove_box 2 3 |> gen_box 256 2 3

let grid_tests = [
  "address on empty cell" >:: (fun _ ->
      assert_equal None (address 2 3 grid1));
  "content_box on empty cell is 0" >:: (fun _ -> 
      assert_equal 0 (grid1 |> address 0 0 |> content_box));
  "content_box on filled cell gives value of filled cell" >:: (fun _ ->
      assert_equal 2 (grid2_t |> address 0 0 |> content_box));
  "[box_of_cell |> value] gives value of filled cell" >:: (fun _ ->
      assert_equal 2 (grid2_t |> address 0 0 |> box_of_cell |> value));
  "remove changes filled cell to empty cell" >:: (fun _ ->
      assert_equal None (grid2 () |> remove_box 0 0 |> address 0 0));
  "remove leaves empty cell unchanged" >:: (fun _ ->
      assert_equal None (grid1 |> remove_box 1 2 |> address 1 2));
  "pos behaves like the inverse of address" >:: (fun _ ->
      assert_equal (2, 1) (gridl () |> address 2 1 |> box_of_cell |> pos));
  "test to_matrix" >:: (fun _ -> 
      assert_equal [[2; 0; 0; 0]; [0; 0; 0; 0]; [0; 0; 0; 2048]; [0; 0; 0; 0]]
        (to_matrix gridw));
  "check win condition - false" >:: (fun _ ->
      assert_equal false (win grid2_t));
  "check lose condition - false" >:: (fun _ ->
      assert_equal false (lose grid2_t));
  "check win condition - true" >:: (fun _ ->
      assert_equal true (win gridw));
  "check lose condition - empty cell in center" >:: (fun _ ->
      assert_equal false (lose grid3_t));
  "check lose condition - empty cell in rightmost column" >:: (fun _ ->
      assert_equal false (lose grid4));
  "check lose condition - empty cell in bottom row" >:: (fun _ ->
      assert_equal false (lose grid5));
  "check lose condition - horizontal duplicate in middle" >:: (fun _ ->
      assert_equal false (lose grid6));
  "check lose condition - vertical duplicate in middle" >:: (fun _ ->
      assert_equal false (lose grid7));
  "check lose condition - horizontal duplicate in rightmost column" >::
  (fun _ -> assert_equal false (lose grid8));
  "check lose condition - horizontal duplicate in bottommost row" >:: (fun _ ->
      assert_equal false (lose grid9));
  "check lose condition - vertical duplicate in bottommost row" >:: (fun _ ->
      assert_equal false (lose grid10));
  "check lose condition - vertical duplicate in rightmost row" >:: (fun _ ->
      assert_equal false (lose grid11));
  "check lose condition - true" >:: (fun _ ->
      assert_equal true (lose gridl_t));
  "copies are equal" >:: (fun _ ->
      assert_equal grid5 (Grid.copy grid5));
  "original unchanged when copy is altered" >:: (fun _ ->
      let memory = Grid.copy grid5 in
      let _ = (Grid.copy grid5) |> gen_box 16 3 2 in 
      assert_equal memory grid5);
  "testing string_rep" >:: (fun _ -> 
      assert_equal 
        ("----------------------------"^
         "\n|2|4|8|16|\n|32|64|128|256|\n|512|1024|2|0|\n|4|8|16|32|")
        (string_rep grid4));
]

let st1 = new_state grid5 1256 "game log"
let st2 = new_state (empty ()) 625 ""
let g_m1c () = empty () |> gen_box 2 0 0 |> gen_box 2 3 3 |> gen_box 4 1 1
let st_m1c () = new_state (g_m1c ()) 0 ""
let g_m1u = empty () |> gen_box 2 0 0 |> gen_box 2 0 3 |> gen_box 4 0 1
let st_m1u = new_state g_m1u 0 ""
let g_m1d = empty () |> gen_box 2 3 0 |> gen_box 2 3 3 |> gen_box 4 3 1
let st_m1d = new_state g_m1d 0 "" 
let g_m1l = empty () |> gen_box 2 0 0 |> gen_box 2 3 0 |> gen_box 4 1 0
let st_m1l = new_state g_m1l 0 ""  
let g_m1r = empty () |> gen_box 2 0 3 |> gen_box 2 3 3 |> gen_box 4 1 3
let st_m1r = new_state g_m1r 0 ""
let g_m21 () = empty () |> gen_box 2 2 2 |> gen_box 2 3 2
let st_m21 () = new_state (g_m21 ()) 0 ""
let g_m22 () = empty () |> gen_box 4 1 0 |> gen_box 4 1 2 
let st_m22 () = new_state (g_m22 ()) 0 ""
let g_m3u () = empty () |> gen_box 4 0 1 |> gen_box 2 2 1
let st_m3u () = new_state (g_m3u ()) 0 "" 
let g_m3d () = empty () |> gen_box 4 3 1 |> gen_box 2 2 1
let st_m3d () = new_state (g_m3d ()) 0 "" 
let g_m3l () = empty () |> gen_box 4 2 0 |> gen_box 2 2 1
let st_m3l () = new_state (g_m3l ()) 0 "" 
let g_m3r () = empty () |> gen_box 4 2 3 |> gen_box 2 2 1
let st_m3r () = new_state (g_m3r ()) 0 "" 

let state_tests = [
  "test grid getter" >:: (fun _ ->
      assert_equal grid5 (grid st1));
  "test score getter" >:: (fun _ ->
      assert_equal 1256 (score st1));
  "test game log getter" >:: (fun _ ->
      assert_equal "game log" (gamelog st1));
  "copies are equal" >:: (fun _ -> 
      assert_equal st1 (copy st1));
  "update score changes score accordingly" >:: (fun _ ->
      update_score st1 1200; 
      assert_equal 1200 (score st1));
  "original unchanged when copy is altered" >:: (fun _ -> 
      let memory = copy st2 in 
      update_score st2 1300; 
      assert_equal 625 (score memory));
  "boxes stop moving when they hit an edge (up)" >:: (fun _ ->
      assert_equal st_m1u (move_all (st_m1c ()) U));
  "boxes stop moving when they hit an edge (down)" >:: (fun _ ->
      assert_equal st_m1d (move_all (st_m1c ()) D));
  "boxes stop moving when they hit an edge (left)" >:: (fun _ ->
      assert_equal st_m1l (move_all (st_m1c ()) L));
  "boxes stop moving when they hit an edge (right)" >:: (fun _ ->
      assert_equal st_m1r (move_all (st_m1c ()) R));
  "handles merge of adjacent boxes (up)" >:: (fun _ -> 
      let g = empty () |> gen_box 4 0 2 in 
      let st = new_state g 4 "" in 
      assert_equal st (move_all (st_m21 ()) U));
  "handles merge of adjacent boxes (up)" >:: (fun _ -> 
      let g = empty () |> gen_box 4 3 2 in 
      let st = new_state g 4 "" in 
      assert_equal st (move_all (st_m21 ()) D));
  "handles merge of non-adjacent boxes (left)" >:: (fun _ -> 
      let g = empty () |> gen_box 8 1 0 in 
      let st = new_state g 8 "" in 
      assert_equal st (move_all (st_m22 ()) L));
  "handles merge of non-adjacent boxes (right)" >:: (fun _ -> 
      let g = empty () |> gen_box 8 1 3 in 
      let st = new_state g 8 "" in 
      assert_equal st (move_all (st_m22 ()) R));
  "boxes stop moving when colliding with box of different value (up)" >:: 
  (fun _ -> 
     let g = g_m3u () |> remove_box 2 1 |> gen_box 2 1 1 in 
     let st = new_state g 0 "" in
     assert_equal st (move_all (st_m3u ()) U));
  "boxes stop moving when colliding with box of different value (right)" >:: 
  (fun _ -> 
     let g = g_m3r () |> remove_box 2 1 |> gen_box 2 2 2 in 
     let st = new_state g 0 "" in
     assert_equal st (move_all (st_m3r ()) R));
  "boxes stop moving when colliding with box of different value (down)" >::
  (fun _ -> assert_equal (st_m3d ()) (move_all (st_m3d ()) D));
  "boxes stop moving when colliding with box of different value (left)" >::
  (fun _ -> assert_equal (st_m3l ()) (move_all (st_m3l ()) L));
]

let g_p1d1_1x () = empty () |> gen_box 32 0 3 |> gen_box 4 3 0 |> gen_box 4 3 2
                   |> gen_box 32 3 3 
let st_p1d1_1x () = new_state (g_p1d1_1x ()) 0 "" 
let g_p1d1_1y1 () = empty () |> gen_box 4 0 0 |> gen_box 4 0 2 
                    |> gen_box 64 0 3
let st_p1d1_1y1 () = new_state (g_p1d1_1y1 ()) 64 "" 
let g_p1d1_1y2 () = empty () |> gen_box 4 3 0 |> gen_box 4 3 2 
                    |> gen_box 64 3 3 
let st_p1d1_1y2 () = new_state (g_p1d1_1y2 ()) 64 "" 
let g_p1d1_2x () = empty () |> gen_box 32 1 1 |> gen_box 16 1 3 
                   |> gen_box 16 2 0 |> gen_box 8 2 1 |> gen_box 16 3 0 
                   |> gen_box 8 3 1 
let st_p1d1_2x () = new_state (g_p1d1_2x ()) 0 "" 
let g_p1d1_2y1 () = empty () |> gen_box 32 2 1 |> gen_box 32 3 0 
                    |> gen_box 16 3 1 |> gen_box 16 3 2 
let st_p1d1_2y1 () = new_state (g_p1d1_2y1 ()) 48 "" 
let g_p1d1_2y2 () = empty () |> gen_box 32 0 0 |> gen_box 32 0 1 
                    |> gen_box 16 0 3 |> gen_box 16 1 1 
let st_p1d1_2y2 () = new_state (g_p1d1_2y2 ()) 48 "" 
let g_p1d2_1x () = empty () |> gen_box 2 0 1 |> gen_box 16 0 3 
                   |> gen_box 16 3 0 |> gen_box 8 3 1 |> gen_box 2 3 2
let st_p1d2_1x () = new_state (g_p1d2_1x ()) 0 "" 
let g_p1d2_1y () = empty () |> gen_box 16 0 0 |> gen_box 2 0 1 |> gen_box 2 0 2
                   |> gen_box 16 0 3 |> gen_box 8 1 1 
let st_p1d2_1y () = new_state (g_p1d2_1y ()) 0 "" 
let g_p1d2_2x () = empty () |> gen_box 8 0 1 |> gen_box 2 0 3 |> gen_box 4 1 0
                   |> gen_box 4 1 3 |> gen_box 8 3 1 |> gen_box 2 3 2 
                   |> gen_box 16 3 3 
let st_p1d2_2x () = new_state (g_p1d2_2x ()) 0 "" 
let g_p1d2_2y () = empty () |> gen_box 8 0 0 |> gen_box 2 0 1 |> gen_box 8 1 0
                   |> gen_box 8 3 0 |> gen_box 2 3 1 |> gen_box 16 3 2 
let st_p1d2_2y () = new_state (g_p1d2_2y ()) 8 "" 
let g_p2d1_1x () = empty () |> gen_box 4 0 1 |> gen_box 8 1 1 |> gen_box 8 3 1
let st_p2d1_1x () = new_state (g_p2d1_1x ()) 0 ""
let g_p2d1_1y () = g_p2d1_1x () |> gen_box 2 2 1
let st_p2d1_1y () = new_state (g_p2d1_1y ()) 0 ""
let g_p2d1_2x () = empty () |> gen_box 2 1 0 |> gen_box 2 1 3 |> gen_box 4 2 1
                   |> gen_box 4 2 3 
let st_p2d1_2x () = new_state (g_p2d1_2x ()) 0 ""
let g_p2d1_2y1 () = g_p2d1_2x () |> gen_box 2 2 1 
let st_p2d1_2y1 () = new_state (g_p2d1_2y1 ()) 0 ""
let g_p2d1_2y2 () = g_p2d1_2x () |> gen_box 2 2 2
let st_p2d1_2y2 () = new_state (g_p2d1_2y2 ()) 0 ""
let g_p2d1_3x () = empty () |> gen_box 8 0 1 |> gen_box 32 0 2 |> gen_box 8 2 0
                   |> gen_box 8 2 3 |> gen_box 8 3 1 |> gen_box 32 3 2 
let st_p2d1_3x () = new_state (g_p2d1_3x ()) 0 ""
let g_p2d1_3y () = g_p2d1_3x () |> gen_box 2 2 2
let st_p2d1_3y () = new_state (g_p2d1_3y ()) 0 ""
let g_p2d2_1x () = empty () |> gen_box 4 0 0 |> gen_box 2 0 1 |> gen_box 4 0 2
                   |> gen_box 2 1 0 |> gen_box 64 1 1 |> gen_box 2 1 2 
                   |> gen_box 64 3 2
let st_p2d2_1x () = new_state (g_p2d2_1x ()) 0 ""
let g_p2d2_1y () = g_p2d2_1x () |> gen_box 2 2 1 
let st_p2d2_1y () = new_state (g_p2d2_1y ()) 0 ""
let g_p2d2_2x () = empty () |> gen_box 16 0 0 |> gen_box 32 1 0 
                   |> gen_box 4 1 1 |> gen_box 16 1 3 |> gen_box 32 3 0
                   |> gen_box 8 3 2 |> gen_box 2 3 3 |> gen_box 16 3 1
let st_p2d2_2x () = new_state (g_p2d2_2x ()) 0 ""
let g_p2d2_2y () = g_p2d2_2x () |> gen_box 2 2 0
let st_p2d2_2y () = new_state (g_p2d2_2y ()) 0 ""

let (>=<) st1 st2 = 
  (grid st1 = grid st2) && (score st1 = score st2)

let npc_tests = [
  "cpu_d1 blocks merge" >:: (fun _ ->
      assert ((st_p2d1_1y ()) >=< (cpu_d1 (st_p2d1_1x ()))));
  "cpu_d1 blocks biggest immediate merge" >:: (fun _ ->
      assert ((st_p2d1_2y1 ()) >=< (cpu_d1 (st_p2d1_2x ())) || 
              (st_p2d1_2y2 ()) >=< (cpu_d1 (st_p2d1_2x ()))));
  "cpu_d1 chooses to block multiple merges if possible" >:: (fun _ ->
      assert ((st_p2d1_3y ()) >=< (cpu_d1 (st_p2d1_3x ()))));
  "cpu_d2 blocks a possible merge one move ahead" >:: (fun _ -> 
      assert ((st_p2d2_1y ()) >=< (cpu_d2 (st_p2d2_1x ()))));
  "cpu_d2 blocks a immediate merge instead if it results in a higher score 
    change" >:: (fun _ -> 
      assert ((st_p2d2_2y ()) >=< (cpu_d2 (st_p2d2_2x ()))));
  "cpu_p1 (d = 1) merges boxes with highest value" >:: (fun _ -> 
      assert ((((st_p1d1_1y1 ()) >=< (cpu_p1 1 (st_p1d1_1x ())))) || 
              ((((st_p1d1_1y2 ()) >=< (cpu_p1 1 (st_p1d1_1x ())))))));
  "cpu_p1 (d = 1) goes for biggest merge even if it involve merges multiple 
    smaller boxes" >:: (fun _ ->
      assert ((((st_p1d1_2y1 ()) >=< (cpu_p1 1 (st_p1d1_2x ())))) ||
              ((((st_p1d1_2y2 ()) >=< (cpu_p1 1 (st_p1d1_2x ())))))));
  "cpu_p1 (d = 2) creates a merge opportunity next turn if none are available 
    in current turn" >:: (fun _ -> 
      assert ((st_p1d2_1y ()) >=< (cpu_p1 2(st_p1d2_1x ()))));
  "cpu_p1 (d = 2) does not merge for the sake of an even bigger merge in the 
    next turn" >:: (fun _ ->
      assert ((st_p1d2_2y ()) >=< (cpu_p1 2 (st_p1d2_2x ()))));
]
(* Loading in account with empty values *)
let sampleacc1 = "sample_acc.json"
let sample_jsn1 = Yojson.Basic.from_file sampleacc1
let sam_acc1 = account_rep_of_json sample_jsn1

(* Loading in account with non-empty values *)
let sampleacc2 = "sample2_acc.json"
let sample_jsn2 = Yojson.Basic.from_file sampleacc2
let sam_acc2 = account_rep_of_json sample_jsn2

let account_test = [
  (* testing the empty values: lower bound edge case *)
  "sample_acc: check last_score 0" >:: (fun _ ->
      assert_equal 0 (last_score sam_acc1));
  "sample_acc: check empty title" >:: (fun _ ->
      assert_equal "" (title sam_acc1));
  "sample_acc: check empty date" >:: (fun _ ->
      assert_equal "" (date_joined sam_acc1));
  "sample_acc: check empty last_played" >:: (fun _ ->
      assert_equal "" (last_played sam_acc1));
  "sample_acc: check 0 games played" >:: (fun _ ->
      assert_equal 0 (games_played sam_acc1));
  "sample_acc: check name" >:: (fun _ ->
      assert_equal "sample" (name sam_acc1));
  "sample_acc: check 0 last_score" >:: (fun _ ->
      assert_equal 0 (last_score sam_acc1));
  "sample_acc: check empty all_scores" >:: (fun _ ->
      assert_equal "" (all_scores sam_acc1));

  (* testing the account with non-empty values *)
  "sample_acc2: check last_score" >:: (fun _ ->
      assert_equal 16 (last_score sam_acc2));
  "sample_acc2: check title" >:: (fun _ ->
      assert_equal "Trying out non-empty values" (title sam_acc2));
  "sample_acc2: check date_joined" >:: (fun _ ->
      assert_equal "12/7/2019" (date_joined sam_acc2));
  "sample_acc2: check last_played" >:: (fun _ ->
      assert_equal "12/7/2019" (last_played sam_acc2));
  "sample_acc2: check games played" >:: (fun _ ->
      assert_equal 2 (games_played sam_acc2));
  "sample_acc2: check name" >:: (fun _ ->
      assert_equal "sample2" (name sam_acc2));
  "sample_acc2: check last_score" >:: (fun _ ->
      assert_equal 16 (last_score sam_acc2));
  "sample_acc2: check all_scores - non-duplicate" >:: (fun _ ->
      assert_equal "16,8" (all_scores sam_acc2));
  "sample_acc2: check highest score - no duplicate" >:: (fun _ ->
      assert_equal 16 (max_number_list (
          let str_lst = String.split_on_char ',' (all_scores sam_acc2) in
          str_to_int_lst str_lst)));
]

let suite = "search test suite" >::: List.flatten[
    grid_tests;
    state_tests;
    npc_tests;
    account_test;
  ]

let _ = run_test_tt_main suite


