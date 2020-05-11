open Graphics
open Board
open Game
open State
open Command

(** Raised when there an invalid file is inputted *)
exception InvalidFile

(** Raised when there an invalid number of lines is inputted *)
exception InvalidLineNum

(** [run_game game lines st] runs the game from the original game file 
    [game], the number of lines to be cleared to win [lines], and the 
    current state of the game [st] *)
let rec run_game game lines st = 
  try let x = keyboard game st in 
    x |> update game |> run_game game lines with 
  | NoKeyPress -> run_game game lines (update game st)
  | _ -> let again = end_keyboard () in if again 
    then (make_window (); start_game game lines) else () 

(** [start_game game lines] starts a Tetris game from the game file [game]
    that requires [num_lines] number of lines cleared to win *)
and start_game game num_lines = 
  run_game game num_lines (init_state game num_lines)

(** [read_file_helper ()] is the parsed game file from the json file chosen by 
    the user. The user will be reprompted if they enter an invalid json file. *)
let rec read_file_helper () =
  let file_name =
    match read_line () with
    | exception End_of_file -> raise InvalidFile
    | read_name -> read_name in
  try 
    let game = file_name |>  Yojson.Basic.from_file |> parse in game
  with 
  | Sys_error s -> 
    ANSITerminal.(print_string [yellow] "Please enter a valid Tetris mode:\n");
    ANSITerminal.(print_string [cyan] "tetris.json or pentris.json\n\n");
    print_string  "> ";
    read_file_helper ()

(** [main ()] prompts the user to choose a file of blocks to load, makes the 
    window, and starts the game engine.*)
let main () = 
  ANSITerminal.(print_string [red] "\n\nWelcome to OCaml Tetris.\n");
  ANSITerminal.(print_string [yellow] 
                  "Please enter the name of the Tetris mode you want to play:\n");
  ANSITerminal.(print_string [cyan] 
                  "tetris.json or pentris.json\n\n");
  print_string  "> ";
  let game = read_file_helper () in
  ANSITerminal.(print_string [magenta] 
                  "\nPlease enter the number of lines to win a game.\n\n");
  print_string  "> ";
  let lines_to_win =
    match read_line () with
    | exception End_of_file -> raise InvalidLineNum
    | lines -> int_of_string lines in
  make_window ();
  start_game game lines_to_win

(** Executes the Tetris game engine. *)
let () = main ()