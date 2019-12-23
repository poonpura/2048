(**
   Representation of grid data structure.

   This module represents the data stored in 4x4 grid, including
   the individuals and the boxes.
*)

(** The abstract type of the 4x4 grid *)
type t

(** The type of a box *)
type box

(**  The type of an individual cell in the grid. *)
type cell = box option

(**[row a t] is the row [a]-th row of grid [t] *)
val row : int -> t -> cell array

(** [address a b g] returns the cell of location ([a],[b]) in grid [g], where
    [a] is the row number and [b] is the column number.
    Requires: a, b in 0..3 *)
val address : int -> int -> t -> cell

(** [gen_box v a b g] is a [g] with box of value [v] in cell with
    position (a, b).
    Requires: cell at position (a, b) is empty. *)
val gen_box: int -> int -> int -> t -> t

(** [remove_box a b g] is [g] with cell location (a,b) as None
*)
val remove_box: int -> int -> t -> t

(** [value bx] is the value associated with [bx] *)
val value : box -> int

(**[pos b] is the tuple location of box [b] in the grid *)
val pos: box -> int * int

(** [empty ()] is the empty grid. *)
val empty : unit -> t

(** [box_of_cell (Some box)] is [box].
    Raise: Failure if not box *)
val box_of_cell: box option -> box

(** [to_matrix t] is the int list list representation of grid [t], where the 
    contents of the empty cell are represented by value 0. *)
val to_matrix: t -> int list list

(** [random t] is a new grid a 2 placed in any empty cell. If the grid is full,
    then return the same grid *)
val random: t -> t

(** [win g] is true iff it has a cell containing the box with value 2048.
*)
val win : t -> bool

(** [lose g] is true iff none of the cells are empty and no two adjacent
    boxes have the same value. *)
val lose : t -> bool

(** [content_box a b] is the content of [a] if [a] is Some int and is 0 otherwise *)
val content_box : box option -> int

(** [copy g] is a copy of [g] such that any changes made to [copy g] will leave
    [g] unaffected and vice versa. *)
val copy : t -> t

(** [string_rep g] is the string representation of [g]. *)
val string_rep : t -> string


