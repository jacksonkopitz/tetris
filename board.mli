(** Representation of the Tetris board and window.*)

(** The x-coordinate of the lower left corner of the Tetris board. *)
val startx : int

(** The y-coordinate of the lower left corner of the Tetris board. *)
val starty : int

(** The width of the Tetris board. *)
val boardw : int

(** The height of the Tetris board. *)
val boardh : int

(** The color of the grid lines and the moving block shadow. *)
val darkgrey : int

(** The size of the sub-block that create Tetris pieces. *)
val tilesize : int

(** [create_board ()] draws the tetris board and grid. *)
val create_board : unit -> unit

(** [make_window ()] creates the Graphics window, renders the board and 
    directions, and sets up the Lines Remaining, Time, and Queue markers. *)
val make_window : unit -> unit