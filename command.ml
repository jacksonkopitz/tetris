open Graphics
open Board
open Game
open State
open Unix

exception NoKeyPress
exception GameOver

let keyboard game st =
  let status = wait_next_event [Poll] in 
  if status.keypressed then
    (match read_key () with 
     | 'd' ->  move "right" st
     | 'a' -> move "left" st
     | 'w' -> rotate "clockwise" st game
     | 'x' -> rotate "counterclockwise" st game
     | ' ' -> begin try drop st with
         | GameWon -> raise GameOver 
         | GameLost -> raise GameOver end
     | 'c' -> hold st
     | 's' -> begin try soft_drop st with
         | GameWon -> raise GameOver 
         | GameLost -> raise GameOver end
     | _ -> raise NoKeyPress)
  else raise NoKeyPress

let rec end_keyboard () =
  let status = wait_next_event [Poll] in 
  if status.keypressed then
    (match read_key () with 
     | 'y' -> true
     | _ -> end_keyboard ())
  else end_keyboard ()

