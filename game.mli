(** Representation of Tetris game settings. This module will go through data stored
    in the JSON files, load it, and allow querying. The data is information 
    in game setting such as tile shape, color, and possible rotating positions.
    This module was influenced by CS 3110 assignment A2/A3 *)

(** The type of orientation names *)
type orientation_name = string

(** The type of shape names *)
type shape_name = string

(** The type of a coordinate *)
type coordinate

(** The type of an orientation *)
type orientation

(** The type of a shape *)
type shape

(** The abstract type of values representing a tetris game. *)
type t

(** [parse j] is a record of a tetris game from [j]
    Raises [Type_error] when [j] is a wrong type of json element *)
val parse : Yojson.Basic.t -> t

(** [get_shapes j] is the [shape] list from [j] *)
val get_shapes : t -> shape list

(** [get_shape_name shape] is the [shape_name] of [shape] *)
val get_shape_name : shape -> shape_name

(** [shape_color shape] is the color of [shape] *)
val shape_color : shape option -> int

(** [shape_orientations shape] is the [orientation] list for [shape] *)
val shape_orientations : shape option -> orientation list

(** [orientation_name orientation] is the [orientation_name] of [orientation] *)
val orientation_name : orientation option -> orientation_name

(** [orientation_coordinates orient] is a [coordinate] list for the 
    coordinates in [orientation] *)
val orientation_coordinates : orientation option -> coordinate list 

(** [coord_x coord] is the x coordinate of [coord] *)
val coord_x : coordinate -> int

(** [coord_y coord] is the y coordinate of [coord] *)
val coord_y : coordinate -> int

(** [rand_shape t] is a random [shape] in tetris game [t] *)
val rand_shape : t -> shape

(** [orientation_init shpe] is the first orientation, [Some orientation], of 
    [shpe] and [None] if [shpe] has no orientations *)
val orientation_init : shape -> orientation option

(** [shape_height shpe] is the height of [shpe] *)
val shape_height : shape -> int

(** [next_orientation direction game shape curr_orientation] is the next
    [orietnation] given the [direction] the [shape] is rotating and its
    current orientation [curr_orientation] and the game information in [game] *)
val next_orientation : string -> t -> shape option -> 
  orientation option -> orientation option