

open State
open Grid
open Yojson.Basic.Util

type t = {
  row0 : string;
  row1 : string;
  row2 : string;
  row3 : string;
  score : int;
  gamelog : string;
}

(**[str_list row] is the string representation of int list [row]*)
let rec str_list row =
  match row with
  |[] -> ""
  |h::t -> string_of_int(h) ^ (str_list t)

let string_save st =
  let score = score st in
  let grid_lst = to_matrix (grid st) in
  let row0 = str_list (List.nth grid_lst 0) in
  let row1 = str_list (List.nth grid_lst 1) in
  let row2 = str_list (List.nth grid_lst 2) in
  let row3 = str_list (List.nth grid_lst 3) in
  `Assoc [ ("row0", `String row0); ("row1"), `String row1;
           ("row2", `String row2); ("row3"), `String row3;
           "score", `Int score; ("gamelog", `String (gamelog st))]

let state_rep_of_json j = {
  row0 = j |> member "row0" |> to_string;
  row1 = j |> member "row1" |> to_string;
  row2 = j |> member "row2" |> to_string;
  row3 = j |> member "row3" |> to_string;
  score = j |> member "score" |> to_int;
  gamelog = j |> member "gamelog" |> to_string
}
(* n is the length of string *)
let rec str_to_intlst str n =
  match str with
  |"" -> []
  |_ -> [int_of_string(String.sub str 0 1)] @ 
        str_to_intlst (String.sub str 1 (n-1)) (n-1)

(**[gen_row] is a grid with row [b] filled in grid [g]*)
let rec gen_row a b lst g =
  match lst with
  |[] -> g
  |h::t -> if (h = 0) then gen_row a (b+1) t g
    else gen_row a (b+1) t (gen_box h a b g)


let create_state sf =
  let row0 = str_to_intlst sf.row0 4 in
  let row1 = str_to_intlst sf.row1 4 in
  let row2 = str_to_intlst sf.row2 4 in
  let row3 = str_to_intlst sf.row3 4 in
  let saved_g = empty() in
  let g = (gen_row 0 0 row0 saved_g |> gen_row 1 0 row1 |> gen_row 2 0 row2
           |> gen_row 3 0 row3) in
  new_state g sf.score sf.gamelog