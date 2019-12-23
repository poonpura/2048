(** FUNCTIONS FOR CPU-CONTROLLED PLAYER (AI UNITS) *)

open State
open Grid
open Command

(** [cpu_d0 st] is a copy of [st] with a box generated corresponding to
    difficulty level 0 for single-player mode. In difficulty level 0, the CPU
    generates a box in a random empty cell. *)
let cpu_d0 st =
  let g = (st|>grid|>random) in
  new_state g (score st) ((gamelog st) ^ "\n\nCPU's move: " ^ (string_rep g))

let best_tile_d1_dir g i j dir =
  let Some box1 = (address i j g) in
  let (di, dj) =
    match dir with
    | U -> (-1, 0)
    | D -> (1, 0)
    | L -> (0, -1)
    | R -> (0, 1)
  in
  match address (i + di) (j + dj) g with
  | Some _ -> []
  | None ->
    begin
      match address (i + 2 * di) (j + 2 * dj) g with
      | Some box2 ->
        if value box1 = value box2 then
          [(value box1, (i + di, j + dj))]
        else
          []
      | None ->
        let edge_cond =
          begin
            match dir with
            | U -> i = 3
            | D -> i = 0
            | L -> j = 3
            | R -> j = 0
          end
        in
        if edge_cond then
          begin
            match address (i + 3 * di) (j + 3 * dj) g with
            | Some box2 ->
              if (value box1) = (value box2) then
                [(value box1, (i + di, j + dj));
                 (value box1, (i + 2 * di, j + 2 * dj))]
              else
                []
            | None -> []
          end
        else []
    end

(** [best_of_tile_d1 g i j] is [(sc, (r, c))] where [r] and [c] is the row and
    column to place a box to prevent a merge with box in cell (i, j) in [g] and
    [sc] is the score of the merge that would be prevented if a box were placed
    at the corresponding location. If there is no such position, outputs
    (0, (0, 0)). *)
let best_tile_d1 g i j =
  match address i j g with
  | None -> []
  | Some box ->
    let vert =
      if i < 2 then
        best_tile_d1_dir g i j D
      else
        best_tile_d1_dir g i j U
    in
    let hori =
      if j < 2 then
        best_tile_d1_dir g i j R
      else
        best_tile_d1_dir g i j L
    in
    vert @ hori

let comp_addr (_, (i1, j1)) (_, (i2, j2)) =
  let i_comp = compare i1 i2 in
  if i_comp = 0 then
    compare j1 j2
  else
    i_comp

let merge_pos acc (v1, (i1, j1)) =
  match acc with
  | [] -> [(v1, (i1, j1))]
  | (v2, (i2, j2)) :: t as lst ->
    if 
      (i1, j1) = (i2, j2) 
    then
      (v1 + v2, (i1, j1)) :: t
    else
      (v1, (i1, j1)) :: lst

let get_max arr =
  let lst_fl = arr |> Array.to_list |> List.flatten |> List.sort comp_addr |>
               List.fold_left merge_pos [] in
  let max = List.fold_left (fun acc x -> if fst x > fst acc then x else acc)
      (0, (0, 0)) lst_fl in
  if max = (0, (0, 0)) then None else Some max

(** [cpu_d1 st] is a copy of [st] with a box generated corresponding to
    difficulty level 1 for single-player mode. In difficulty level 1, the CPU
    attempts to block the biggest preventable merge possible in the next player
    move. *)
let cpu_d1 st =
  let g = grid st in
  let options = Array.make 16 [] in
  let _ = for i = 0 to 3 do
      for j = 0 to 3 do
        options.(4 * i + j) <- best_tile_d1 g i j
      done
    done in
  let g_new =
    match get_max options with
    | None -> random g
    | Some (_, (i, j)) -> gen_box 2 i j g
  in
  new_state g_new (score st) 
    ((gamelog st) ^ "\n\nCPU's move: " ^ (string_rep g_new))

let delta_score st dir = 
  let clone = State.copy st in 
  let sim = move_all clone dir in 
  (score sim) - (score st)

let delta_score2 st dir =
  let open List in
  let clone = State.copy st in 
  let sim = move_all clone dir in
  let delta1 = (score sim - score st) in
  let dirs = [U; D; L; R] in 
  let deltas = map (fun x -> delta_score sim x) dirs in 
  let dict = combine deltas dirs in 
  let (max :: max2 :: _) = deltas |> sort compare |> rev in 
  delta1 + (max + max2) / 2

(** TODO: DOCUMENT. *)
let cpu_p1 d st = 
  let open List in
  let dirs = [U; D; L; R] in
  let f = 
    if d = 1 then delta_score 
    else if d = 2 then delta_score2 
    else failwith "Invalid difficulty"
  in
  let deltas = map (fun x -> f st x) dirs in 
  let dict = combine deltas dirs in
  let (max :: _) = deltas |> sort compare |> rev in  
  let best_dir = assoc max dict in 
  move_all st best_dir

let score_d2 i j st =
  match (st |> grid |> address i j) with 
  | Some _ -> max_int
  | None ->
    let clone = State.copy st in 
    let g_sim1 = clone |> grid |> gen_box 2 i j in 
    let sim1 = new_state g_sim1 (score clone) (gamelog clone) in 
    let sim2 = sim1 |> cpu_p1 2 |> cpu_p1 2 in 
    score sim2

(** [cpu_d2 st] is a copy of [st] with a box of value 2 added to the grid by 
    the CPU corresponding to difficulty level 2. This algorithm attempts to 
    minimize the score increase by 2 consecutive moves made by [cpu_p1]. *)
let cpu_d2 st = 
  let acc = ref [] in 
  for i = 0 to 3 do 
    for j = 0 to 3 do 
      acc := ((score_d2 i j st), (i, j)) :: !acc
    done 
  done; 
  let dict = !acc in 
  let sorted = List.sort_uniq (fun (x, _) (y, _) -> compare x y) dict in 
  let g =
    match sorted with 
    | ((_, (i, j))) :: _ :: _ :: _ -> st |> grid |> gen_box 2 i j
    | _ :: _ :: [] -> st |> grid |> random
    | _ -> failwith "bad input"
  in
  new_state g (score st) ((gamelog st) ^ "\n\nCPU's move: " ^ (string_rep g))
