(**This file is used to load/save the game from/to a JSON representation.*)


(**This abstract type that is supposed to represent the game state. This
   include rows [0...3], score and gamelog. Hence, by loading in the JSON
   file, it we can parse it into this type t and from then on extract the 
   necessarycomponent to build up a game state *)  
type t

(**[string_save g] creates a JSON representation of the grid g*)  
val string_save :State.t ->
  [> `Assoc of (string * [> `Int of int | `String of string ]) list ]

(**[create_state t] is the game State of the loaded type [t] representation
   of the game file*) 
val create_state : t -> State.t

(**[state_rep_of_json jsn] is type [t] representation of the loaded game*)
val state_rep_of_json : Yojson.Basic.t -> t