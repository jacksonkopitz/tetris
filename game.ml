open Yojson.Basic.Util

type orientation_name = string

type shape_name = string

type coordinate = {
  x : int;
  y : int 
}

type orientation = {
  orient_name : orientation_name;
  coordinates : coordinate list
}

type shape = {
  shape_name : shape_name;
  color : int;
  orientations : orientation list;
}

type t = {
  shapes : shape list
}

(** [coordinates_of_json j] is a record of ao coordinate from [j] *)
let coordinates_of_json j = {
  x = j |> member "x" |> to_int;
  y = j |> member "y" |> to_int;
}

(** [orientation_of_json j] is a record of an orientation from [j] *)
let orientation_of_json j = {
  orient_name = j |> member "oname" |> to_string;
  coordinates = 
    j |> member "coordinates" |> to_list |> List.map coordinates_of_json;
}

(** [shape_of_json j] is a record of a shape from [j] *)
let shape_of_json j = {
  shape_name = j |> member "name" |> to_string;
  color = j |> member "color" |> to_string |> int_of_string;
  orientations = 
    j |> member "orientations" |> to_list |> List.map orientation_of_json;
}

(** [game_of_json j] is a record of a tetris game from [j] *)
let game_of_json j = {
  shapes = j |> member "tiles" |> to_list |> List.map shape_of_json;
}

let parse j =
  try game_of_json j 
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let get_shapes j =
  j.shapes

let get_shape_name shape = 
  shape.shape_name

let shape_color shape = 
  match shape with 
  | None -> 0
  | Some thing -> thing.color

let shape_orientations shape = 
  match shape with 
  | None -> []
  | Some thing -> thing.orientations

let orientation_name orientation = 
  match orientation with 
  | None -> "none"
  | Some x -> x.orient_name

let orientation_coordinates orient =
  match orient with 
  | None -> []
  | Some thing -> thing.coordinates

let coord_x coord = coord.x

let coord_y coord = coord.y

(** [rand_helper shapes idx] is the [shape] in [shapes] at index [idx]. 
    Raises error if [idx] is too larger to index into [shapes] *)
let rec rand_helper shapes idx = 
  match shapes, idx with
  | h::t, x when x = 0 -> h
  | h::t, _ -> rand_helper t (idx-1)
  | [], _ -> failwith ("error with random shape indexing")

let rand_shape t = 
  let shapes = t.shapes in
  let length = List.length shapes in
  let idx = Random.int length in
  rand_helper shapes idx

let orientation_init shpe = 
  match shpe.orientations with
  | [] -> None
  | h::t -> Some h

(** [shape_height_helper lst min_y max_y] is height of shape given
    its coordinates in [lst] and the initialized [min_y] and [max_y] *)
let rec shape_height_helper lst min_y max_y =
  match lst with
  | [] -> max_y - min_y + 1
  | h::t -> 
    let y = coord_y h 
    in shape_height_helper t (min y min_y) (max y max_y)

let shape_height shpe = 
  match (orientation_init shpe) with
  | None -> failwith "no orientations"
  | Some o -> shape_height_helper o.coordinates 1 (-1)

(** [get_shape_helper shapes shape] is the [shape] named [shape_name] in 
    the [shapes]  *)
let rec get_shape_helper shapes shape_name = 
  match shapes with
  | [] -> failwith "shape not found"
  | h::t -> if h.shape_name = shape_name then h 
    else get_shape_helper t shape_name

(** [get_shape game shape_name] is the [shape] with [shape_name] in [game] *)
let get_shape game shape_name = 
  get_shape_helper game.shapes shape_name

(** [get_first lst] is the first element in [lst] *)
let get_first lst = 
  match lst with 
  | [] -> failwith "empty list"
  | h::t -> h

(** [next_orientation_helper o_list curr_orientation] is the [orientation]
    in [o_list] named [curr_orientation] *)
let rec next_orientation_helper o_list curr_orientation = 
  match o_list with
  | [] -> failwith "current orientation not found"
  | h::x::t -> if h.orient_name = curr_orientation then x 
    else next_orientation_helper (x::t) curr_orientation
  | h::t -> failwith "current orientation not found"

(** [next_shape direction orientation_list curr_orientation] is 
    the next orientation in [orientation_list] given the [direction]
    of rotation and the current orientation name [curr_orientation] *)
let next_shape direction orientation_list curr_orientation =
  let o_list = 
    if direction = "clockwise" then (orientation_list@orientation_list)
    else (List.rev orientation_list)@(List.rev orientation_list) in
  next_orientation_helper o_list curr_orientation

let rec next_orientation direction game shape curr_orientation = 
  match shape with 
  | None -> failwith "no shape yet"
  | Some s -> match curr_orientation with
    | None -> failwith "no orientaiton yet"
    | Some o -> 
      Some (next_shape direction 
              (get_shape game s.shape_name).orientations o.orient_name)
