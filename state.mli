(**
   Representation of dynamic 2048-game state.

   This module represents the state of the game as it is being played,
   mainly the grid, but also the current score, and functions that cause the
   state to change.

   The state also contains a game log, which records the grids after each player
   or NPC has made their move. At the end of the game, the game log is to be 
   exported and the player(s) can review their game through it. 
*)

(** The abstract type of values representing the game state. *)
type t

(**The type of values representing direction *)
type dir = U | D | L | R

(** [init_state ()] is initial state of the game. In that state the grid will
    have 2 random non-empty cells each containing a box of value 2. *)
val init_state : unit -> t

(** [grid st] is the current grid of the state. *)
val grid : t -> Grid.t

(** [score st] is the current score of the state. *)
val score : t -> int

(** [gamelog st] is the string representing the game log of st since it was 
    initialized. *)
val gamelog : t -> string

(** [new_state g s gamelog] *)
val new_state : Grid.t -> int -> string -> t

(** [update_score st s] changes the score field of [st] to s. *)
val update_score : t -> int -> unit

(** [move_all st] is a copy of [st] with the grid being the result of making a
    move in direction [dir] and score updated accordingly to any merges made by 
    the move. *)
val move_all : t -> dir -> t

(** [copy st] is a copy of [st] such that any changes made to [copy g] will 
    leave [st] unaffected and vice versa. *)
val copy : t -> t 