exception Failure

type box = {v: int; pos: int*int}

type cell = box option

type t = cell array list

(**[help_add i count g] returns [i-th] row of grid g*)
let rec help_add i count g =
  match g with
  | h :: t -> if (i = count) then h else help_add i (count+1) t
  | _ -> raise Failure

let row a g =
  help_add a 0 g 

let address a b g =
  (help_add a 0 g).(b)

let box_of_cell = function
  | Some box -> box
  | None -> raise Failure


let gen_box (v:int) a b g =
  (help_add a 0 g).(b) <- Some {v = v; pos = (a,b)}; g

let remove_box a b g =
  (help_add a 0 g).(b) <- None; g

let value box = box.v

let content_box = function
  |None -> 0
  |Some box -> value box

let pos box = box.pos

let empty () =
  [Array.make 4 None; Array.make 4 None; Array.make 4 None; Array.make 4 None]

let rec to_matrix g =
  match g with
  | [] -> []
  | h :: t -> Array.fold_left (fun lst cell -> lst @ [content_box cell]) 
                [] h :: to_matrix t

(**[is_full g] is true if grid [g] is full and false otherwise*)
let is_full g =
  not (List.exists (Array.exists (fun cell -> cell = None)) g)

let random_help = function
  |None -> true
  |Some box -> false

let rec random g =
  let r11 = Random.int 4 in
  let r12 = Random.int 4 in
  if is_full g then g else if 
    (random_help (address r11 r12 g)) then gen_box 2 r11 r12 g
  else random g

(**[win g] returns true there is a box in grid [g] with value 2048*)
let win g =
  List.exists (Array.exists (fun cell -> content_box cell = 2048)) g

(** [l_hori_hlpr row] is true iff there is a mergeable adjacent pair of boxes in 
    [row]. *)
let l_hori_hlpr row =
  let acc = ref false in
  let _ = for i = 0 to 2
    do
      acc := !acc || content_box row.(i) = content_box row.(i + 1)
    done in
  !acc

(** [l_vert helper] is true iff there is a mergeable adjacent pair of boxes in 
    column [i]. *)
let rec l_vert_hlpr i = function
  | h1 :: (h2 :: t) -> 
    content_box h1.(i) = content_box h2.(i) || l_vert_hlpr i (h2 :: t)
  | h1 :: [] -> false
  | [] -> failwith "bad input"

(** [l_vert g] is true iff there is a vertically mergeable pair of boxes in grid
    g.  *)
let l_vert g =
  let acc = ref false in
  let _ = for i = 0 to 3
    do
      acc := !acc || l_vert_hlpr i g
    done in
  !acc


let lose g =
  if List.exists (Array.exists (fun cell -> cell = None)) g then false else
  if List.fold_left (fun acc row -> acc || l_hori_hlpr row) false g then
    false
  else not (l_vert g)

let copy g = 
  let copy_row r = [|r.(0); r.(1); r.(2); r.(3)|] in 
  List.fold_left (fun acc r -> acc @ [copy_row r]) [] g 

(**[string_row row] is the string representation of the row in grid*)
let rec string_row row =
  string_of_int(content_box row.(0)) ^ "|" ^
  string_of_int(content_box row.(1)) ^ "|" ^
  string_of_int(content_box row.(2)) ^ "|" ^
  string_of_int(content_box row.(3)) ^ "|"


let string_rep grid =
  "----------------------------"^
  "\n|" ^ string_row (row 0 grid) ^
  "\n|" ^ string_row (row 1 grid) ^
  "\n|" ^ string_row (row 2 grid) ^
  "\n|" ^ string_row (row 3 grid)
