(**
   Parsing of player commands.
*)

(** The type [command] represents a player command that
    changes the position of the boxes in a grid *)

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

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows.

    Examples:
    - [parse "    w  "] is [Up]
    - [parse "quit"] is [Quit]
    - [parse "a"] is [Left]
    - [parse "s"] is [Down]
    - [parse "d"] is [Right]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if it is neither w,a,s,d or quit*)
val parse : string -> command 