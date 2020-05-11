open Graphics

let startx = 250
let starty = 100
let boardw = 300
let boardh = 600
let darkgrey = rgb 40 40 40
let tilesize = boardh / 20

(** [lines_time_q color] writes "Lines Remaining:", "Time:", and 
    "Queue:" in [color] .*)
let lines_time_q color = 
  moveto 610 700;
  set_color color;
  draw_string ("Lines Remaining:");
  moveto 610 680;
  draw_string ("Time:");
  moveto 610 660;
  draw_string ("Queue:")

(** [clear_window color] fills in the background of the window with [color] .*)
let clear_window color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ())

(** [directions ()] renders the directions for the controls of the game. *)
let directions () = 
  set_color white;
  set_text_size 30;
  moveto 150 50; 
  draw_string ("Use A and D to move left and right. Use W and X to to rotate.");
  moveto 150 20; 
  draw_string ("Press SPACE to drop and S to soft-drop. Press C to hold.")

(** [grid_helper acc total constx consty dx dy] draws in grid lines of 
    length [consty] and width [constx], with spacing width [dx] and height 
    [dy] with a counter [acc] to indicate when [total] pairs have been drawn.*)
let rec grid_helper acc total constx consty dx dy=
  if acc = total then (moveto startx starty) else
    (rmoveto dx dy;
     rlineto constx consty;
     rmoveto dx dy;
     rlineto (-constx) (-consty);
     grid_helper (acc+1) total constx consty dx dy)

let create_board () = 
  moveto startx starty;
  set_color darkgrey;
  grid_helper 0 5 0 boardh tilesize 0;
  grid_helper 0 10 boardw 0 0 tilesize;

  set_color darkgrey;
  lineto startx (starty+boardh);
  lineto (startx+boardw) (starty+boardh);
  lineto (startx+boardw) starty;
  lineto startx starty

let make_window () = 
  open_graph " 800x800";
  set_window_title "tetris";
  clear_window black;
  create_board ();
  directions ();
  lines_time_q white