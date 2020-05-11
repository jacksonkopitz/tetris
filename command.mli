(**
   Interprets and executes player's commands 
*)


(** The exception that is thrown when the user does not input a commant.*)
exception NoKeyPress

(** The exception that is thrown when the game has ended because the user
    has either won or lost.*)
exception GameOver

(** [keyboard game st] is a new state after the user inputs a command
    to modify the current state [st] given the game file [game] *)
val keyboard : Game.t -> State.t -> (State.t)

(** [end_keyboard ()] is [true] if the user has indicated that 
    they want to play another game, and [false] otherwise *)
val end_keyboard : unit -> bool


