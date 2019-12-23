open Grid
type t = {
  current_grid : Grid.t;
  mutable score : int;
  gamelog : string
}

type dir = U|D|L|R

let init_state () =
  let r11 = Random.int 4 in
  let r12 = Random.int 4 in
  let r21 = Random.int 4 in
  let r22 = Random.int 4 in
  let r21 = if (r21 = r11 && r22 = r22) then (r11+1) mod 4 else r21 in
  let grid = empty() in 
  let c_grid = gen_box 2 r11 r12 grid |> gen_box 2 r21 r22 in {
    current_grid = c_grid;
    score = 0;
    gamelog = string_rep (c_grid);
  }

let grid st =
  st.current_grid

let gamelog st =
  st.gamelog

let score st =
  st.score

let new_state  new_grid score gamelog =
  {current_grid = new_grid; score = score; gamelog = gamelog}


let update_score st new_score =
  st.score <- new_score


let rec move_box st dir = function
  | None -> st
  | Some box ->
    let vbox = value box in 
    let (r, c) = pos box in 
    let edge_cond = 
      match dir with 
      | U -> r = 0 
      | D -> r = 3
      | L -> c = 0
      | R -> c = 3
    in
    if edge_cond then st else
      let grid = grid st in
      let (dr, dc) = 
        match dir with
        | U -> (-1, 0)
        | D -> (1, 0)
        | L -> (0, -1)
        | R -> (0, 1)
      in 
      match address (r + dr) (c + dc) grid with 
      | None -> grid |> gen_box vbox (r + dr) (c + dc) |> remove_box r c 
                |> address (r + dr) (c + dc) |> move_box st dir 
      | Some box2 -> 
        if value box2 = value box then 
          let new_v = 2 * (value box) in
          update_score st ((score st) + new_v);
          let new_grid = grid |> remove_box r c |> remove_box (r + dr) (c + dc)
                         |> gen_box new_v (r + dr) (c + dc)
          in 
          new_state new_grid (score st) (gamelog st)
        else
          st 

let fold_dir dir =
  fun f st ->
  let acc = ref st in 
  (match dir with 
   | U -> 
     for i = 0 to 3 do 
       for j = 0 to 3 do 
         acc := !acc |> grid |> address i j |> f
       done
     done
   | D -> 
     for i = 3 downto 0 do 
       for j = 0 to 3 do 
         acc := !acc |> grid |> address i j |> f
       done
     done
   | L ->
     for j = 0 to 3 do 
       for i = 0 to 3 do 
         acc := !acc |> grid |> address i j |> f
       done
     done
   | R -> 
     for j = 3 downto 0 do 
       for i = 0 to 3 do 
         acc := !acc |> grid |> address i j |> f
       done
     done);
  !acc

let move_all st dir =
  (fold_dir dir) (move_box st dir) st 

let copy st = {
  current_grid = Grid.copy st.current_grid;
  score = st.score;
  gamelog = st.gamelog
}











