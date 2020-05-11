open Graphics
open Board
open Game

(** the spawn point of the blocks. *)
let orig_blockref = (startx + (boardw/2), starty + boardh -tilesize)

(** Raised when there are no more blocks left in the queue. *)
exception NoMoreBlocks

exception GameWon

exception GameLost

type t = {
  blockref : int * int; 
  moving_block : Game.shape option;
  hold : Game.shape option; 
  current_orientation : Game.orientation option;
  time : int;
  queue : Game.shape list;
  won : bool;
  dropped : (int array) array;
  animate : float;
  rows_left : int;
}

let won st = st.won  

let moving_block st = st.moving_block

let hold_st st = st.hold

let current_orientation st = st.current_orientation

let time_st st = st.time

let queue st = st.queue

let rows_left st = st.rows_left

let dropped st = st.dropped

(** [init_q length acc t] initializes a [Game.shape list] of random shapes 
    of length [length] including the list [acc] given game information in [t] *)
let rec init_q length acc t = 
  match length with 
  | n when n = 0 -> acc
  | _ -> init_q (length-1) ((rand_shape t)::acc) t

(** [start] is a reference to the time that the game has started *)
let start = 
  ref (Unix.time ())

(** [time st] is the amount of time in seconds since the game has started. 
    This function will also update the time on the game board. *)
let time st =
  set_color black;
  fill_rect 640 680 100 10;  
  set_color white;
  set_text_size 30;
  moveto 650 680; 
  let time = int_of_float((Unix.time () -. !start)) in
  draw_string (string_of_int time);
  time

let init_state t lines = 
  start := (Unix.time ());
  {
    blockref = orig_blockref;
    moving_block = None;
    hold = None;
    current_orientation = None;
    time = int_of_float(!start);
    queue = init_q 5 [] t;
    won = false;
    dropped = Array.make_matrix 10 20 0;
    animate = Unix.time ();
    rows_left = lines; 
  }

let blockref_x st = 
  match st.blockref with
  | (x, _) -> x

let blockref_y st = 
  match st.blockref with
  | (_, y) -> y

(** [curr_row st] is the row location of the render point of [st]'s 
    currently moving block in the 10x20 array.*)
let curr_row st =
  (blockref_y st - starty) / tilesize

(** [curr_col st] is the cloumn location of the render point of [st]'s 
    currently moving block in the 10x20 array.*)
let curr_col st = 
  let pixels = (blockref_x st)- startx in (pixels/tilesize)

(** [coord_to_pix axis num] converts index [num] on the [axis] axis of the 
    10x20 array to pixel coordinates.*)
let coord_to_pix axis num = 
  if axis = "x" 
  then startx + (num * tilesize)
  else starty + (num * tilesize)

(** [convert_blk_to_pix_coor st lst acc] is a list of pixel coordinates 
    calculated from the [blockref] value in [st] and the block coordinates
    from [lst] combined with the pixel coordinates in [acc] *)
let rec convert_blk_to_pix_coor st lst acc = 
  match lst with
  | [] -> acc
  | h::t -> convert_blk_to_pix_coor st t 
              (((blockref_x st)+((coord_x h)*tilesize),
                (blockref_y st)+((coord_y h)*tilesize))::acc)

(** [add-blockref st num1 num2] is an [(int * int)] containing the current 
    blockref of [st], but with [num1] added to the first value, and [num2] 
    added to the second.*)
let add_blockref st num1 num2 =
  match st.blockref with
  | (x, y) -> (x+num1, y+num2)

(** [render_block refx refy color orientation] draws a [color]-colored tile 
    with coordinates in [orientation] at coordinates [refx, refy]. *)
let render_block refx refy color orientation =  
  let rec helper coords =
    match coords with
    | [] -> ()
    | h::t -> 
      let x = coord_x h in
      let y = coord_y h in
      if (refy + y * tilesize) < 700 then
        (fill_rect (refx + x * tilesize) (refy + y * tilesize) tilesize tilesize;
         helper t)
      else 
        helper t
  in 
  set_color color; 
  helper (orientation_coordinates orientation)

(** [render_array dropped x y] fills the index in [dropped] with it's color, 
    starting at [x, y].*)
let rec render_array dropped x y = 
  if y > 19 
  then () 
  else (
    if dropped.(x).(y) = 0 
    then 
      if (x=9 && y=19) 
      then ()
      else 
      if y=19 
      then render_array dropped (x+1) 0
      else (render_array dropped x (y+1))
    else 
    if (x=9 && y=19) 
    then fill_rect (x*tilesize+startx) (y*tilesize+starty) tilesize tilesize
    else 
    if y=19 
    then (set_color dropped.(x).(y); fill_rect (x*tilesize+startx) 
            (y*tilesize+starty) tilesize tilesize; render_array dropped (x+1) 0)
    else (set_color dropped.(x).(y); fill_rect (x*tilesize+startx) 
            (y*tilesize+starty) tilesize tilesize; render_array dropped x (y+1)
         ))

(** [erase_block refx refy orientation] redraws the background grid at the 
    coordinates in [orientation] beginning at render point [refx, refy].*)
let erase_block refx refy orientation = 
  let rec helper coords =
    match coords with
    | [] -> ()
    | h::t -> 
      let x = coord_x h in
      let y = coord_y h in
      if (refy+y*tilesize) < (starty + boardh) then
        (set_color black;
         fill_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
         set_color darkgrey;
         draw_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
         helper t)
      else helper t
  in set_color darkgrey; 
  helper (orientation_coordinates orientation)


(** [erase_lines_remaining ()] redraws the window over the Lines Remaining. *)
let erase_lines_remaining () = 
  set_color black;
  fill_rect 710 700 30 100 

(** [render_lines_remaining num] draws [num] into the board at the Lines
    Remanining measurement.*)
let render_lines_remaining num =
  erase_lines_remaining ();
  set_color white;
  set_text_size 30;
  moveto 710 700;
  draw_string (string_of_int num);
  num

(** [erase_q ()] draws a black box over the tile queue before it is redrawn *)
let erase_q () = 
  set_color black;
  fill_rect 560 0 200 650

(** [render_q q dx dy] draws a tile queue of the shapes in [q] at 
    position [(dx, dy)] *)
let rec render_q q dx dy =
  match q with
  | [] -> ()
  | h::t -> render_block dx dy (shape_color (Some h)) (orientation_init h); 
    render_q t dx (dy-(tilesize*(1+(shape_height h))))

(** [pop queue game] is tuple of the a tile and a queue. It also will 
    erase the current queue, draw [queue], and raise [NoMoreBlocks] if [queue]
    is empty *)
let pop queue game = 
  match queue with 
  | x::t -> let q = (t@(rand_shape game::[])) in 
    erase_q () ; render_q q 640 570; (x, q)
  | [] -> raise NoMoreBlocks

(** [find_lowest_y dropped column] finds the highest cell that is filled in 
    with part of a tile at column [column] of array [dropped]*)
let find_lowest_y dropped column =
  let rec find_lowest_y_helper column idx = 
    if idx < 0 then -1 else 
      match column.(idx) with
      | n when n > 0 -> (idx)
      | _ -> find_lowest_y_helper column (idx-1) in
  find_lowest_y_helper dropped.(column) 19

(** [add_coordinate dropped x y color] set coordinate ([x],[y]) to [color] 
    in the array [dropped] *)
let add_coordinate dropped x y color = 
  dropped.(x).(y) <- color

(** [can_remove dropped y x] is [true] if there is no 0 valued element in row 
    [y] starting at position [x] in the array [dropped] and [false] otherwise *)
let rec can_remove dropped y x =
  if dropped.(x).(y) > 0 then 
    if x = 9 then (true, dropped)
    else can_remove dropped y (x+1)
  else (false, dropped)

(** [make_val dropped y x value] sets the element at position 
    ([x], [y]) in the array [dropped] to [value] *)
let make_val dropped y x value = 
  dropped.(x).(y) <- value;
  dropped

(** [rem_row dropped y x] will replace all values beginning at ([x],[y]) 
    with the value at position ([x],[y+1]) to remove the row [y]. 
    Values with positions will be initialised to 0 if their [y] 
    cooridnate is 19. *)
let rec rem_row dropped y x =
  if y = 19 then 
    if x = 9 
    then make_val dropped y x 0
    else let dr = make_val dropped y x 0 in rem_row dr y (x+1)
  else 
  if x = 9 
  then let dr2 = make_val dropped y x dropped.(x).(y+1) in
    rem_row dr2 (y+1) 0
  else let dr3 = make_val dropped y x dropped.(x).(y+1) in 
    rem_row dr3 y (x+1)

(** [row_remove_helper dropped pos rows_removed] will remove the full 
    rows in [dropped] starting at the y coordinate [pos] and returns the 
    number of rows that have been removed in addition to [rows_removed] *)
let rec row_remove_helper dropped pos rows_removed = 
  if pos < 20 then
    match (can_remove dropped pos 0) with
    | (true, dr) -> 
      let dr2 = (rem_row dr pos 0) in
      row_remove_helper dr2 (pos+1) (rows_removed+1)
    | (false, dr) -> row_remove_helper dr (pos+1) (rows_removed)
  else (rows_removed, dropped)

let row_remove st =
  let (new_rows_removed, drop) = row_remove_helper st.dropped 0 0 in
  {
    blockref = st.blockref; 
    moving_block = st.moving_block;
    hold = st.hold; 
    current_orientation = st.current_orientation;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = drop;
    animate = st.animate;
    rows_left = st.rows_left - new_rows_removed;
  }

(** [leftmost_corrd acc lst] is [acc] the leftmost coordinate in the list [lst] *)
let rec leftmost_coord acc lst = 
  match lst with
  | [] -> acc
  | (x,y)::t -> if x < acc 
    then leftmost_coord x t else leftmost_coord acc t

(** [rightmost_corrd acc lst] is [acc] the rightmost coordinate in the list [lst] *)
let rec rightmost_coord acc lst = 
  match lst with
  | [] -> acc
  | (x,y)::t -> if x > acc 
    then rightmost_coord x t else rightmost_coord acc t

let hold st = 
  erase_block (blockref_x st) (blockref_y st) st.current_orientation; 
  set_color black;
  fill_rect 0 400 250 275;
  render_block 100 500 (shape_color st.moving_block) st.current_orientation;
  let new_current_shape = {
    blockref = add_blockref st 0 0;
    moving_block = st.hold;
    hold = st.moving_block;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
    current_orientation = 
      match st.hold with 
      | Some h -> orientation_init h
      | None -> None}  in 
  render_block (blockref_x new_current_shape) (blockref_y new_current_shape) 
    (shape_color new_current_shape.moving_block) 
    new_current_shape.current_orientation; new_current_shape

(** [display_win_message time] with erase the game board and display a
    winning message including [time], the game time to finish the game, in 
    addition to giving the user the option to intialize a new game *)
let display_win_message time =
  set_color black;
  fill_rect 0 0 800 800;
  set_color white;
  moveto 240 370;
  draw_string 
    ("Congratulations! You win! Your time is "^(string_of_int time)^". Nice job.");
  moveto 310 350;
  draw_string ("Press 'y' to play again.")

(** [display_lose_message] with erase the game board and display a
    lose message and give the user the option to intialize a new game *)
let display_lose_message () =
  set_color black;
  fill_rect 0 0 800 800;
  set_color white;
  moveto 352 370;
  draw_string 
    ("You lose!");
  moveto 310 350;
  draw_string ("Press 'y' to play again.")

(** [win time rows_left] is [rows_left], but if there are no rows left,
    the winning message display function is called with parameter [time] *)
let win time rows_left = 
  if rows_left > 0
  then rows_left
  else (display_win_message time; 
        rows_left)

(** [parse_dropped st dropped coords curr_col] is the tuple containing the
    y-value of the highest cell in [dropped] within the width of a block with 
    render point [curr_col] and coordinates [coords] and y-coordinate of that 
    block all given the current state of the game [st] *)
let parse_dropped st dropped coords curr_col =
  let rec helper dropped doords acc=  
    match doords with 
    | [] -> acc
    | h::t -> 
      let x = coord_x h in 
      let y = coord_y h in 
      let temp_col = curr_col + x in
      let temp = find_lowest_y dropped temp_col in 
      let diff = (curr_row st) + y - 1 -(temp + 1) in
      let updated = 
        if diff = (fst acc) 
        then
          if y > (snd (snd acc)) then acc else (diff, (temp, y))
        else 
        if diff < (fst acc) then (diff, (temp, y)) else acc in 
      helper dropped t updated in snd (helper dropped coords (20, (-4, -4)))

(** [add_to_dropped dropped color coords target_cell y_target_coord curr_col]
    fills in the blocks corresponding to the coordinates in [coords] with color 
    [color]. [target_cell], [y_target_coord], and [curr_col] are measures to 
    help calculate proper block placement, representing the y-value of the 
    highest cell in [dropped] within the width of a block with x-value render 
    point [curr_col] and coordinates [coords], y-coordinate of that block, 
    and x-value of the render point, respectively.*)
let rec add_to_dropped dropped color coords target_cell y_target_coord curr_col= 
  match coords with 
  | [] -> ()
  | h::t -> 
    let x = coord_x h in 
    let y = coord_y h in 
    add_coordinate dropped (curr_col+x) (target_cell-y_target_coord+y) color;
    add_to_dropped dropped color t target_cell y_target_coord curr_col

(** [update_after_row_rem drop x y] will use the colors in [drop] to 
    draw a block at ([x], [y]) for the all coordinates above and to the right 
    of ([x], [y]) *)
let rec update_after_row_rem drop x y  = 
  set_color drop.(x).(y);
  fill_rect (x*tilesize+startx) (y*tilesize+starty) tilesize tilesize;
  if x > 8 then 
    if y > 18 then () else update_after_row_rem drop 0 (y+1)
  else update_after_row_rem drop (x+1) y

(** [new_st st] is state [st] altered that removes the moving block and resets
    its render point to the original value. *)
let new_st st= 
  {
    blockref = orig_blockref;
    moving_block = None;
    hold = st.hold;
    current_orientation = None;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped= st.dropped;
    animate = st.animate;
    rows_left = render_lines_remaining st.rows_left;
  }

let drop st = 
  let color = shape_color st.moving_block in
  let coords = orientation_coordinates st.current_orientation in
  let curr_col = curr_col st in 
  let (target_cell, y_target_coord) = 
    parse_dropped st st.dropped coords curr_col in
  add_to_dropped st.dropped color coords (target_cell+1) y_target_coord curr_col;
  erase_block (blockref_x st) (blockref_y st) st.current_orientation;
  render_array st.dropped 0 0;
  let final_res = row_remove (new_st st) in
  update_after_row_rem final_res.dropped 0 0;
  if (win final_res.time final_res.rows_left) > 0 
  then final_res 
  else raise GameWon

(** [rotate_helper_left string st game] is [st] altered with the 
    current orientation updated to be the next orientation of the block 
    determined by whichever direction [string] the user types when the 
    block is at the rightmost end of the screen in the game [game] *)
let rotate_helper_left string st game = 
  let shifted_left_shape = {
    blockref = add_blockref st (-tilesize) 0;
    moving_block = st.moving_block;
    hold = st.hold;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
    current_orientation = 
      next_orientation string game st.moving_block st.current_orientation } 
  in erase_block (blockref_x st) (blockref_y st) st.current_orientation; 
  shifted_left_shape

(** [rotate_helper_right string st game] is [st] altered with the 
    current orientation updated to be the next orientation of the block determined 
    by whichever direction [string] the user types when the block is at the 
    leftmost end of the screen in the game [game] *)
let rotate_helper_right string st game = 
  let shifted_right_shape = {
    blockref = add_blockref st tilesize 0;
    moving_block = st.moving_block;
    hold = st.hold;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
    current_orientation = 
      next_orientation string game st.moving_block st.current_orientation } 
  in erase_block (blockref_x st) (blockref_y st) st.current_orientation; 
  shifted_right_shape

let rotate string st game = 
  let pixel_list = convert_blk_to_pix_coor st 
      (orientation_coordinates st.current_orientation) [] in 
  if (leftmost_coord (blockref_x st) pixel_list) <= startx then  
    rotate_helper_right string st game else 
  if (rightmost_coord (blockref_x st) pixel_list) >= startx + boardw - tilesize 
  then rotate_helper_left string st game
  else  let new_shape = {
      blockref = add_blockref st 0 0;
      moving_block = st.moving_block;
      hold = st.hold;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
      animate = st.animate;
      rows_left = st.rows_left;
      current_orientation = 
        next_orientation string game st.moving_block st.current_orientation
    } in erase_block (blockref_x st) (blockref_y st) st.current_orientation; 
    new_shape

let move direction st =
  let pixel_list = 
    convert_blk_to_pix_coor st 
      (orientation_coordinates st.current_orientation) [] in 
  if ((leftmost_coord (blockref_x st) (pixel_list)) <= startx 
      && direction = "left") then st
  else if (rightmost_coord (blockref_x st) (pixel_list)) 
          >= (startx+boardw) - tilesize  
       && direction = "right" 
  then st else 
    let new_shape = {
      blockref = add_blockref st (if direction = "right" then 
                                    tilesize else (-tilesize)) 0;
      moving_block = st.moving_block;
      hold = st.hold;
      current_orientation = st.current_orientation;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped= st.dropped;
      animate = st.animate;
      rows_left = st.rows_left } in 
    erase_block (blockref_x st) (blockref_y st) st.current_orientation; new_shape

(** [create_shape game st final_res] is a new state whose moving block has been 
    randomly generated from the list of blocks in [game], [final_res] is the 
    state with the proper number of rows
    remaining.*)
let create_shape game st final_res = 
  let (new_shape, new_queue) = pop st.queue game in 
  {
    blockref = st.blockref;
    moving_block = Some new_shape;
    hold = st.hold;
    current_orientation = orientation_init new_shape;
    time = time st;
    queue = new_queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = render_lines_remaining final_res.rows_left;
  }

(** [create_shape st final_res] is a [st] altered whose moving block has been moved 
    down one tile. [final_res] is the state with the proper number of rows
    remaining.*)
let move_down st final_res= 
  {
    blockref = if (Unix.time ()) -. st.animate = 1. 
      then add_blockref st 0 (-tilesize) 
      else st.blockref;
    moving_block = st.moving_block;
    hold = st.hold;
    current_orientation = st.current_orientation;
    time = time st;
    queue = st.queue;
    won = (if st.rows_left = 0 then true else false);
    dropped = st.dropped;
    animate = if (Unix.time ()) -. st.animate = 1. 
      then (Unix.time ()) 
      else st.animate;
    rows_left = render_lines_remaining final_res.rows_left;
  }

let update game st = 
  try
    let final_res = row_remove st in
    update_after_row_rem final_res.dropped 0 0;
    let result = 
      if st.moving_block = None then create_shape game st final_res
      else move_down st final_res in 
    let coords = orientation_coordinates st.current_orientation in
    let curr_col = curr_col st in 
    let (target_cell, y_target_coord) = 
      parse_dropped st st.dropped coords curr_col in
    if (curr_row st) - 1 + y_target_coord <= target_cell then drop result else
      (if (Unix.time ()) -. st.animate = 1. 
       then erase_block (blockref_x st) (blockref_y st) st.current_orientation;
       render_block (blockref_x st) 
         (coord_to_pix "y" (target_cell + 1 - y_target_coord)) 
         (darkgrey) st.current_orientation;
       render_block (blockref_x result) (blockref_y result)
         (shape_color st.moving_block) st.current_orientation; 
       create_board ();
       result)
  with 
  | Invalid_argument s ->
    display_lose_message (); raise GameLost

let soft_drop st = 
  {
    blockref = add_blockref st 0 (-tilesize) ;
    moving_block = st.moving_block;
    hold = st.hold;
    current_orientation = st.current_orientation;
    time = time st;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
  }




