(**This module represents the account that is to be created and includes
   relevant functions necessary to query specific statistics about each 
   account *)

(**Abstract representation of the account of the player*)
type t

(**[account_str st name score played dj lp sc title] puts the data associated
   with the account into an associated list so that it can be parsed into a
   json file*)
val account_str : State.t ->
  string ->
  'a ->
  'a ->
  string ->
  string ->
  string ->
  string -> [> `Assoc of (string * [> `Int of 'a | `String of string ]) list ]

(**[account_rep_of_json jsn] is the type [t] representation of the 
   json account data file*)
val account_rep_of_json : Yojson.Basic.t -> t

(**[games_played t] is the number of games played by the user *)
val games_played : t -> int

(**[name t] is the name of the account *)
val name : t -> string

(**[last_score t] is the lastest score received by the user of this account*)
val last_score : t -> int

(**[date_joined t] is the day the user created the account*)
val date_joined : t -> string

(**[last_played t] is the day the user last played the game*)
val last_played : t -> string

(**[last_played t] is all the scores earned by the player*)
val all_scores : t -> string

(**[title t] is the title that the player self-prescribed*)
val title : t -> string

(**[str_to_int_lst str] is the int list representation the [str] list
   RI: elements of [str] must be a string representation of integers*)
val str_to_int_lst : string list -> int list


(**[max_number_list lst] is the largest number in list [lst] *)
val max_number_list : int list -> int