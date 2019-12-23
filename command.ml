(* TODO: command module structure *)
type command =
  | Up
  | Down
  | Left
  | Right
  | Quit
  | Player2 of int * int
  | GameMode1
  | GameMode2
  | GameMode3
  | Difficulty1
  | Difficulty2
  | Difficulty3
  | Scorelog
  | TimeMode
  | Load 
  | Save
  | Statistics 

exception Empty
exception Malformed


(* [parse_aux cleanlst] is a helper function that returns output of type command
   Raises [Empty] if cleanlst is empty
   Raises [Malformed] if input is not formatted according
   to the specification of parse*)
let parse_aux cleanlst =
  match cleanlst with
  | []-> raise Empty
  | h::t -> if h = "w" && List.length t = 0 then Up
    else if h = "quit" && List.length t = 0 then Quit 
    else if h = "a" && List.length t =0 then Left 
    else if h = "s" && List.length t = 0 then Down 
    else if h = "d" && List.length t =0 then Right
    else if h = "p" && List.length t = 2 then 
      Player2 (int_of_string (List.nth t 0), int_of_string (List.nth t 1))
    else if h = "single" && List.length t = 0 then GameMode1 
    else if h = "multi" && List.length t = 0 then GameMode2
    else if h = "d0" && List.length t = 0 then Difficulty1 
    else if h = "d1" && List.length t = 0 then Difficulty2 
    else if h = "d2" && List.length t = 0 then Difficulty3
    else if h = "reverse" && List.length t = 0 then GameMode3 
    else if h = "timemode" && List.length t = 0 then TimeMode
    else if h = "scorelog" && List.length t = 0 then Scorelog
    else if h = "load" && List.length t = 0 then Load 
    else if h = "save" && List.length t = 0 then Save
    else if h = "statistics" && List.length t = 0 then Statistics
    else raise Malformed



(* [clean_input split_input lst] is a helper function
   that returns a list of the user input with all white space removed*)
let rec clean_input split_input lst =
  match split_input with
  | [] -> []
  | h::t -> if h = "" then clean_input t lst
    else List.rev_append [h] (clean_input t lst)

let parse str =

  let split_input = String.split_on_char ' ' str in

  let clean_list = clean_input split_input [] in

  parse_aux clean_list