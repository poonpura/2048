open State
open Grid
open Command
open Printf
open Pervasives
open Yojson.Basic.Util
open Savefile
open Account
open Npc

type dir = State.dir


(**[time ()] is the string representation of hte current time*)
let time () = 
  let cur_time_sec = (Unix.time ()) in 
  let final_time = Unix.localtime cur_time_sec in 
  let year = string_of_int (final_time.tm_year +1900) in 
  let month = string_of_int (final_time.tm_mon +1) in
  let date = string_of_int final_time.tm_mday in
  month^"/"^date^"/"^year

(**[update_leaderboard () name score] updates the scorelog with new [name] and
   [score]*)
let update_leaderboard () name score = 
  let message = String.concat " " [">";name;"-";score;"<"] in 
  let file = "scorelog.txt" in
  let oc = open_out_gen [Open_append] 0 file in 
  (* fprintf oc "%s\n" "\n"; *)
  fprintf oc "%s\n" message; 
  close_out oc; 
  exit 0

(**[final_details state] passes user inputted [name] into [update_leaderboard]*)
let final_details state = 
  print_endline "Please enter your name to be inputted into leader board"; 
  let name = read_line() in 

  let final_score = string_of_int (score state) in 
  update_leaderboard () name final_score 

(**[get_dj fn] is the date_joined of the account type [fn]*)
let get_dj fn =
  date_joined fn

(**[outacc jsn name state] output a json representation
   of the [name]'s account*)
let outacc jsn name state =
  (* ANSITerminal.(print_string [red] 
     ("Enter account name to be saved as\n")); *)
  let savename = name^"_acc" in
  if (Sys.file_exists savename) then 
    let acc_jsn = Yojson.Basic.from_file savename in
    let acc = account_rep_of_json acc_jsn in
    let dj = get_dj acc in
    let title = title acc in
    let score = score state in
    let score_log = string_of_int(score) ^ "," ^ (all_scores acc) in
    let games_played = (games_played acc) + 1 in
    let jsn = account_str state name score 
        games_played dj (time()) score_log title in
    let jsnfile = open_out (savename) in
    output_string jsnfile (Yojson.Basic.pretty_to_string jsn);
    final_details state 
  else
    let jsnfile = open_out savename in
    output_string jsnfile (Yojson.Basic.pretty_to_string jsn);
    final_details state 

(**[parse_acc state] calls [outacc] if the user wants to save account
   and [final_details] otherwise*)
let parse_acc state =
  print_endline "Do you want to save your record for this account? Y/N";
  let ans = read_line () in
  if (ans = "Y" or ans = "y") then (
    print_endline "Type in your NetID";
    let acc_name = read_line () in
    print_endline "Please type in your desired title";
    let acc_title = read_line () in
    let jsn = account_str state acc_name (score state) 1 (time()) (time()) 
        (string_of_int(score state)) acc_title in
    outacc jsn acc_name state)
  else final_details state

(*[display grid] prints a 4 by 4 grid with values of the current state *)
let display grid =
  List.iter (fun line ->
      print_endline "----------------------------";
      print_string " |";
      line
      |> List.map (Printf.sprintf "%4d")
      |> String.concat " | "
      |> print_string;
      print_endline "|"

    ) grid

(**[output log] outputs the gamelog [log] into text file*)
let output log =
  let txtfile = open_out "./gamelog.txt" in
  output_string txtfile log;
  close_out txtfile;;

(**[outsave jsn state] outputs the saved [state] into a json file*)
let outsave jsn state = 
  ANSITerminal.(print_string [red] 
                  ("Please enter the name of your save file\n"));
  let savename = read_line() in
  let jsnfile = open_out (savename) in
  output_string jsnfile (Yojson.Basic.pretty_to_string jsn);
  close_out jsnfile;
  parse_acc state;;

(**[parse_save jsn state] calls [outsave] if player agrees to save the file
   and [parse_acc] otherwise*)
let parse_save jsn state = 
  print_endline "Do you want to save the file? 
    Type Y to save and N if you dont want to";
  let ans = read_line () in
  if (ans = "Y" or ans = "y") then outsave jsn state else parse_acc state

(**[new_log st] returns a string representation with the new gamelog added*)
let new_log st =
  ((gamelog st)^ "\n" ^ (string_rep (grid st)))

(*[new_value grid] adds either a 2 or 4 to a random cell containing a zero *)
let exit_time_mode state = 
  print_endline "Sorry you took more than 5 seconds to make a move: GAME OVER";
  parse_save (string_save state) state

(**[p1_phase_timed state] is the game with 5 seconds timer*)
let rec p1_phase_timed state =
  let start_time = int_of_float (Unix.time ()) in 
  let next_move = read_line() in
  try
    if(int_of_float (Unix.time ()) - start_time > 5) then (exit_time_mode state);
    let next_state =
      match(parse next_move) with
      |Quit -> print_endline "thank you for playing"; 
        output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
        parse_save (string_save state) state;
      | Up -> move_all state U
      | Down -> move_all state D
      | Left -> move_all state L
      | Right -> move_all state R
      |_ -> print_endline "invalid player command"; p1_phase_timed state
    in
    let g = grid next_state in
    let scr = score next_state in
    (new_state g scr 
       ((gamelog state)^ "\nP1's move:" ^ (string_rep (grid state))))
  with
  | _ -> print_endline "You did something wrong, please try again"; 
    p1_phase_timed state

(**[p1_phase state] is new the state after p1 plays the game*)
let rec p1_phase state =
  let next_move = read_line() in
  try
    let next_state =
      match(parse next_move) with
      |Quit -> print_endline "thank you for playing"; 
        output ((gamelog state)^ "\n" ^ (string_rep (grid state)));
        parse_save (string_save state) state;
      | Up -> move_all state U
      | Down -> move_all state D
      | Left -> move_all state L
      | Right -> move_all state R
      |_ -> print_endline "invalid player command"; p1_phase state
    in
    let g = grid next_state in
    let scr = score next_state in
    (new_state g scr 
       ((gamelog state)^ "\nP1's move:" ^ (string_rep (grid state))))
  with
  | _ -> print_endline "You did something wrong, please try again"; 
    p1_phase state

(**[p2_phase state] calls [p1_phase state] then [p2_turn state] recursively
   until the exit condition is satisfied*)
let rec p2_phase state =
  print_endline "Player 1's Turn";
  let state2 = p1_phase state in
  display (to_matrix (grid state2));
  let grid2 = grid state2 in
  if win grid2 then 
    (print_endline "Congratulation Player 1. You Win!";
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
     parse_save (string_save state) state;)
  else
    print_endline "Player 2's turn";
  p2_turn state2

(**[p2_turn state2] is the [state] after player 2 plays the game*)
and p2_turn state2 =
  try
    let next_move = read_line() in
    match (parse next_move) with
    |Quit -> print_endline "thank you for playing the game"; 
      final_details state2;
    |Player2 (r, c) -> if (r>3 || r<0 || c>3 || c<0) then (
        display (to_matrix (grid state2)); (
          print_endline "Entered Wrong Command. Player 2 Try again");
        p2_turn state2
      )
      else
      if (0 <> (content_box (address r c (grid state2))))
      then (display (to_matrix (grid state2)); (
          print_endline "The entered location is not empty.Please try again.");
         p2_turn state2
        )
      else
        let fgrid = gen_box 2 r c (grid state2) in
        if lose fgrid then
          (print_endline "Congratulation Player 2. You Win!";
           output ((gamelog state2)^ "\n" ^ (string_rep (grid state2))); 
           parse_save (string_save state2) state2;)
        else 
          (new_state fgrid (score state2) 
             ((gamelog state2)^ "\n\nP2's move:" ^ (string_rep (grid state2))))
    |_ -> display (to_matrix (grid state2)); (
        print_endline "Entered Wrong Command. Player 2 Try again"); 
      p2_turn state2
  with
  |_-> print_endline "You did something wrong, please try again" ; 
    p2_turn state2

(**[match_diff d] calls [cpu_d0] or [cpu_d1] or [cpu_d2] depending on
   player's chosen difficulty*)
let match_diff d = 
  if d = 0 then cpu_d0
  else if d = 1 then cpu_d1
  else if d = 2 then cpu_d2
  else failwith "invalid difficulty"

(**[interface state d] is the interface of the single player mode game*)
let rec interface state d =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let st' = p1_phase state in
  let next_state = (match_diff d) st' in
  let next_grid = grid state in
  if win next_grid then
    (print_endline "Congratulations! You win!"; 
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
     parse_save (string_save state) state;)
  else if lose next_grid then
    (print_endline "Game Over";
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
     parse_save (string_save state) state;)
  else interface next_state d

(**[interface4 state d] is the interface for timed mode*)
let rec interface4 state d =
  display (to_matrix (grid state));
  print_endline (("Current Score: " ^ string_of_int (score state)));
  let st' = p1_phase_timed state in
  let next_state = (match_diff d) st' in
  let next_grid = grid state in
  if win next_grid then
    (print_endline "Congratulations! You win!"; 
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
     parse_save (string_save state) state;)
  else if lose next_grid then
    (print_endline "Game Over";
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
     parse_save (string_save state) state;)
  else interface4 next_state d

(**[interface2 state] is the interface for multiplayer*)
let rec interface2 state =
  display (to_matrix (grid state));
  p2_phase state |> interface2 

(**[rev_phase state d] is the state after player has made their move*)
let rec rev_phase state d =
  print_endline "CPU's Turn";
  let state2 = cpu_p1 d state in
  display (to_matrix (grid state2));
  let grid2 = grid state2 in
  if win grid2 then 
    (print_endline "Game Over"; 
     output ((gamelog state)^ "\n" ^ (string_rep (grid state))); 
     parse_save (string_save state) state;)
  else
    print_endline "Your turn";
  p2_turn state2

(**[interface3 state d] is the interface for reverse mode*)
let rec interface3 state d = 
  display (to_matrix (grid state));
  interface3 (rev_phase state d) d

(**[chose_diff ns] is a function that takes in a state and allows user to 
   chose difficulty for single player game mode*)
let rec chose_diff ns =
  ANSITerminal.(print_string [red] 
                  "type d0 for easy mode and d1 for 
                    medium mode and d2 for hard mode\n");
  let diff_choice = read_line() in
  try
    match (parse diff_choice) with
    | Difficulty1 -> interface (ns) 0
    | Difficulty2 -> interface (ns) 1
    | Difficulty3 -> interface (ns) 2
    | Quit -> exit 0
    | _ -> print_endline "invalid difficulty level, try again"; chose_diff ns
  with
  | Malformed -> print_endline "invalid difficulty level, try again"; 
    chose_diff ns
  | Empty -> print_endline "please enter a difficulty level"; chose_diff ns

(**[chose_diff3 ns] is a function that takes in a state and allows user to 
   choose difficulty for reverse game mode*)
let rec choose_diff3 ns =
  ANSITerminal.(print_string [red] 
                  ("type d0 for easy mode and d1 for hard mode"));
  let diff_choice = read_line () in 
  try
    match (parse diff_choice) with
    | Difficulty1 -> interface3 (ns) 1
    | Difficulty2 -> interface3 (ns) 2
    | Quit -> exit 0
    | _ -> print_endline "invalid difficulty level, try again"; choose_diff3 ns 
  with
  | Malformed -> print_endline "invalid difficulty level, try again"; 
    choose_diff3 ns
  | Empty -> print_endline "please enter a difficulty level"; choose_diff3 ns

(**[chose_diff4 ns] is a function that takes in a state and allows user to 
   choose difficulty for time game mode*)
let rec chose_diff4 ns =
  ANSITerminal.(print_string [red] 
                  ("Type d0 for easy mode, d1 for normal mode and" ^
                   " d2 for hard mode.\nYou will have 5 seconds to 
                    play every move else you lose!"));
  let diff_choice = read_line() in
  try
    match (parse diff_choice) with
    | Difficulty1 -> interface4 (ns) 0
    | Difficulty2 -> interface4 (ns) 1
    | Difficulty3 -> interface4 (ns) 2
    | Quit -> exit 0
    | _ -> print_endline "invalid difficulty level, try again"; chose_diff4 ns
  with
  | Malformed -> print_endline "invalid difficulty level, try again"; 
    chose_diff4 ns
  | Empty -> print_endline "please enter a difficulty level"; chose_diff4 ns

(**[display_time ()] is a function that displays the 
   date that a user logged onto the game, once the game starts*)
let display_time () = 
  print_endline ("You have logged on to the 2048 challenge on: "^ time())

(**[read_file ()] is a function that parses the scorelog file in order to 
   display the scorelog of all 2048 games ever played*)
let rec read_file () = 
  let file = "scorelog.txt" in
  let in_channel = open_in file in
  try
    while true do
      let line = input_line in_channel in
      if (String.contains line '{' = false) && (String.contains line '>') then 
        let final_string = String.sub line (String.index line '>') 
            ((String.index line '<')-(String.index line '>')) in 
        print_endline (final_string);
        (* do something with line *)
    done
  with End_of_file ->
    ANSITerminal.(print_string [red] (
        "Type multi for 2 player game mode or type reverse for reverse mode"^
        "\nType scorelog to see where you stand\n"));
    let game_choice = read_line() in
    match(parse game_choice) with
    | GameMode1 ->  chose_diff (init_state () )
    | GameMode2 -> interface2 (init_state ())
    | Quit -> exit 0
    | _ -> print_endline "You did something wrong, please try again"; 
      read_file ()


(**[chose_diff_reload ()] is a recursive function that 
   allows the user to chose game mode difficulty for a loaded game*)
let rec chose_diff_reload () ns=
  ANSITerminal.(print_string [red] 
                  ("type d0 for easy mode and d1 for normal mode and d2" ^
                   "for hard mode.\n"));
  let diff_choice = read_line() in
  match (parse diff_choice) with
  | Difficulty1 -> interface ns 0
  | Difficulty2 -> interface ns 1
  | Difficulty3 -> interface ns 2
  | Quit -> exit 0
  | _ -> print_endline "invalid difficulty level, try again"; chose_diff ns


(**[main_reload ()] is a recursive function that 
   outputs the 2048 main menu in the event that a user loads a previously 
   played game. It has customized text for this version of the game*)

let rec main_reload ns = 
  ANSITerminal.(print_string [red] (
      "\nYou have loaded a previously attempted game. 
        Type single to resume 1 player mode or " ^ 
      "type multi to resume 2 player game mode 
        or type reverse to resume reverse mode\n"^
      "type timemode to resume the high stress version of the game"
      ^"\n Type load to load some other previously saved game"^
      "\nType scorelog to see where you stand\n"));

  let game_choice = read_line() in
  match(parse game_choice) with
  | GameMode1 ->  chose_diff ns
  | GameMode2 -> interface2 ns
  | GameMode3 -> choose_diff3 ns
  | TimeMode -> chose_diff4 ns
  | Scorelog -> read_file()
  | Load -> load_game() 
  | Quit -> exit 0
  | exception Malformed -> print_endline "you did something wrong, 
    please try again "; main_reload ns 
  | _ -> print_endline "You did something wrong, please try again"

(**[load_game ()] is a recursive function that 
   parses the user input in order to load a previous game 
   state from a json file*)
and load_game () =
  print_endline "Please enter the file that your want to load.";
  let file_to_load = read_line () in 
  let game = Yojson.Basic.from_file file_to_load in 
  let file = state_rep_of_json game in 
  let new_state = create_state file in 
  main_reload new_state


(**[account_stats ()] is a recursive function that 
   displays all the statistical queries possible in 2048 game on utop *)
let rec account_stats () = 
  print_endline ("Please type the account of the person you would 
    want to view the stats of");
  let acc = read_line() in
  let fn = acc^"_acc" in
  if (Sys.file_exists fn) then
    (
      let fn_jsn = Yojson.Basic.from_file fn in
      let account = account_rep_of_json fn_jsn in
      print_endline ("Type name to query the account name. 
        Type score to query last score received. 
        Type num to query the number of games played by the user.
        Type dj to query the date the user created the account.
        Type lp to query the last game played by the user.
        Type all_scores to query all the scores by the player.
        Type highest to query the highest score earned by the player!.
        Type title look at the creative title the user inputted!");
      let choice = read_line() in 
      if (choice = "num") 
      then
        print_endline (string_of_int (games_played account))
      else if (choice = "score")
      then 
        print_endline (string_of_int (last_score account))
      else if (choice = "dj")
      then 
        print_endline (date_joined account)
      else if (choice = "name")
      then
        print_endline (name account)
      else if (choice = "lp")
      then
        print_endline (last_played account)
      else if (choice = "quit") then exit 0
      else if (choice = "title")
      then
        print_endline (title account)
      else if (choice = "all_scores")
      then
        print_endline (all_scores account)
      else if (choice = "highest")
      then
        let sc_strlst = String.split_on_char ',' (all_scores account) in
        let int_lst = str_to_int_lst sc_strlst in
        print_endline ("Highest score: " ^ 
                       string_of_int (max_number_list int_lst));
      else
        print_endline ("You entered a non-existant stat query. 
          Please try again");
      account_stats()
    )

  else if (acc = "quit") then exit 0 

  else
    print_endline ("The account you searched for does not exist"); 
  account_stats()

(**[run_menu ()] is a recursive function that 
   displays the main menu of the 2048 game on utop *)
let rec run_menu () = 

  ANSITerminal.(print_string [red] (
      "\n\nWelcome to the 2048 game."^
      "\nType single for 1 player. " ^ 
      "\nType multi for 2 player game mode." ^
      "\nType reverse for reverse mode."^
      "\nType timemode for the high stress version of the game"^
      "\nType load to load a previously saved game"^
      "\nType scorelog to see where you stand"^
      "\nType statistics to query statistics from our database\n"));

  let game_choice = read_line() in
  match(parse game_choice) with
  | GameMode1 ->  chose_diff (init_state ())
  | GameMode2 -> interface2 (init_state ())
  | GameMode3 -> choose_diff3 (init_state ())
  | TimeMode -> chose_diff4(init_state ())
  | Scorelog -> read_file()
  | Load -> load_game() 
  | Quit -> exit 0
  | Statistics -> account_stats()
  | exception Malformed -> print_endline "you did something wrong, 
    please try again "; run_menu()
  | _ -> print_endline "you did something wrong, please try again"; run_menu () 

let main () =
  display_time();
  run_menu ()

(* Execute the game engine. *)
let () = main ()