(** 
   Functions for the NPC AI

   This file contains the functions for the NPC AI units. These functions 
   have a similar format in which they all take in a game state and output a 
   new game state corresponding to the state of the game after the NPC has made
   its move. 

   The first class of AI units add blocks to the grid each turn. There are 3 
   difficulties: 0, 1, and 2. The general idea of the behavior of the NPC AI 
   for each difficulty is in the specifications of their respective functions.
   The functions corresponding to these units are [cpu_d0], [cpu_d1] and 
   [cpu_d2] respectively. These are used for the single-player mode.

   The second class of AI units take the role of the player of a traditional
   2048 game; they "swipe" the screen to cause the blocks to merge and the 
   score to update accordingly. These are used for the reversed game mode. The 
   function corresponding to this class is [cpu_p1], which is a higher-order
   function. See the specification of this function for details. 
*)

(** [cpu_d0 st] is a copy of [st] with a box generated corresponding to
    difficulty level 0 for single-player mode. In difficulty level 0, the CPU
    generates a box in a random empty cell. *)
val cpu_d0 : State.t -> State.t

(** [cpu_d1 st] is a copy of [st] with a box generated corresponding to
    difficulty level 1 for single-player mode. In difficulty level 1, the CPU
    attempts to block the biggest preventable merge possible in the next player
    move. It also looks for "junctions": if a box placement can block two 
    different merges, the NPC is more likely to make that placement. *)
val cpu_d1 : State.t -> State.t

(** [cpu_d2 st] is a copy of [st] with a box of value 2 added to the grid by 
    the CPU corresponding to difficulty level 2. This algorithm attempts to 
    minimize the score increase by 2 consecutive moves made by [cpu_p1]. If 
    box placement does not affect the score increase, the NPC makes a random
    placement. *)
val cpu_d2 : State.t -> State.t

(** [cpu p1 d st] is a copy of [st] corresponding to the game state after the
    NPC has made a move on [st] in reversed game mode at difficulty [d]. Both
    are greedy algorithms.

    At difficulty [d] = [1], the NPC chooses to move in the direction that
    maximizes the score gain for that move. Intuitively, it looks for all the
    pairs of boxes that can be merged in that move and makes the best decision 
    based on that. 

    At difficulty [d] = [2], the NPC also looks ahead to see whether boxes can
    be moved so that they align and can be merged in the next move. Of course,
    it also weighs this decision against merging any boxes which are mergeable 
    in the current move. In other words, it makes the move the maximizes the 
    score gained over the move and well as the next move. Another idea to think 
    about it is that if there are two boxes of the same value in an otherwise 
    empty grid, this NPC is sure to be able to merge them in 2 moves or less no 
    matter how the boxes are initially aligned. *)
val cpu_p1 : int -> State.t -> State.t