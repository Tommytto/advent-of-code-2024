(* decompose
   1. try_move_box box_point move_vector matrix
   the idea behing the try_move_box is if after target box another box we just continue search using mask
   if we find empty place then swap with target block if no the go next until edge
   if block moved then swap robot also otherwise stop

   2. make a dirs mask
   3. iterate matrix and return list of box coordinates
*)

open Core

type point =
  { x : int
  ; y : int
  }
[@@deriving show]

type robot = { pos : point }
type box = { pos : point }

type vector =
  { dx : int
  ; dy : int
  }

type direction =
  | Left
  | Top
  | Right
  | Bottom
[@@deriving equal]

type char_matrix = char array array [@@deriving equal, show]

let parse_directions str =
  str
  |> Utils.explode
  |> List.filter ~f:(fun c -> not (Char.equal '\n' c))
  |> List.map ~f:(fun dir_char ->
    match dir_char with
    | '<' -> Left
    | '^' -> Top
    | '>' -> Right
    | 'v' -> Bottom
    | _ -> failwith (sprintf "bad direction: %c" dir_char))
;;

let%test _ =
  List.equal
    equal_direction
    (parse_directions "<^>vv")
    [ Left; Top; Right; Bottom; Bottom ]
;;

(* decompose
   1. i got it, it's quite simple after all, we can move box in case if neighours boxes in our directions can be moved as well

   lets split task into 2 parts
   1. can_move_big_box box_part_point vector matrix
   2. move_big_box box_part_point vector matrix
*)

let apply_vector p v matrix =
  let n_y, n_x = p.y + v.dy, p.x + v.dx in
  if Utils.coord_outside matrix (n_y, n_x) then None else Some { y = n_y; x = n_x }
;;

let get_big_box_points (box_part : point) matrix =
  match matrix.(box_part.y).(box_part.x) with
  | '[' -> [ box_part; { y = box_part.y; x = box_part.x + 1 } ]
  | ']' -> [ { y = box_part.y; x = box_part.x - 1 }; box_part ]
  | _ -> failwith "unknown big box part"
;;

let allowed_map_symbols = [ '#'; '.'; '['; ']' ]

let rec can_move_big_box
  matrix
  ?(checked_matrix = Utils.copy_matrix_size matrix false)
  (box_part : point)
  (v : vector)
  =
  let box_points = get_big_box_points box_part matrix in
  List.for_all box_points ~f:(fun p ->
    if checked_matrix.(p.y).(p.x)
    then true
    else (
      checked_matrix.(p.y).(p.x) <- true;
      let n_p_opt = apply_vector p v matrix in
      if Option.is_none n_p_opt
      then false
      else (
        let n_p = Option.value_exn n_p_opt in
        let cell = matrix.(n_p.y).(n_p.x) in
        if equal_char cell '#'
        then false
        else if equal_char cell '.'
        then true
        else if equal_char cell '[' || equal_char cell ']'
        then can_move_big_box matrix ~checked_matrix n_p v
        else failwith "unkown map symbol")))
;;

[@@@ocamlformat "disable"]
let test_matrix_big_box_1 = Utils.char_matrix_of_string "##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##......@...##
##############"
[@@@ocamlformat "enable"]

let%test _ = can_move_big_box test_matrix_big_box_1 { y = 3; x = 5 } { dy = 0; dx = -1 }
let%test _ = can_move_big_box test_matrix_big_box_1 { y = 3; x = 5 } { dy = 0; dx = 1 }
let%test _ = can_move_big_box test_matrix_big_box_1 { y = 3; x = 5 } { dy = 1; dx = 0 }
let%test _ = can_move_big_box test_matrix_big_box_1 { y = 3; x = 5 } { dy = -1; dx = 0 }
let%test _ = can_move_big_box test_matrix_big_box_1 { y = 4; x = 6 } { dy = -1; dx = 0 }
let%test _ = can_move_big_box test_matrix_big_box_1 { y = 4; x = 7 } { dy = -1; dx = 0 }

let rec move_big_box
  matrix
  ?(visited = Utils.copy_matrix_size matrix false)
  ?(visited_check = Utils.copy_matrix_size matrix false)
  b
  v
  =
  if not (can_move_big_box matrix ~checked_matrix:visited_check b v)
  then false
  else (
    let box_points = get_big_box_points b matrix in
    List.for_all box_points ~f:(fun box ->
      if visited.(box.y).(box.x)
      then true
      else (
        visited.(box.y).(box.x) <- true;
        let n_p_opt = apply_vector box v matrix in
        if Option.is_none n_p_opt
        then false
        else (
          let n_p = Option.value_exn n_p_opt in
          if equal_char matrix.(n_p.y).(n_p.x) '['
             || equal_char matrix.(n_p.y).(n_p.x) ']'
          then
            if move_big_box matrix ~visited ~visited_check n_p v
            then (
              Utils.swap_matrix_elem
                matrix
                { y = box.y; x = box.x }
                { y = n_p.y; x = n_p.x };
              true)
            else failwith "unexpected cant move"
          else if equal_char matrix.(n_p.y).(n_p.x) '.'
          then (
            Utils.swap_matrix_elem
              matrix
              { y = box.y; x = box.x }
              { y = n_p.y; x = n_p.x };
            true)
          else failwith "unkown"))))
;;

[@@@ocamlformat "disable"]
let test_matrix_big_box_2 = Utils.char_matrix_of_string "##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##..........##
##############"
[@@@ocamlformat "enable"]

let%test _ =
  let t = Utils.copy_matrix test_matrix_big_box_2 in
  let moved = move_big_box t { y = 4; x = 6 } { dy = 0; dx = 1 } in
  moved
  && equal_char_matrix
       t
       (Utils.char_matrix_of_string
          "##############\n\
           ##......##..##\n\
           ##..........##\n\
           ##...[][]...##\n\
           ##.....[]...##\n\
           ##..........##\n\
           ##############")
;;

let%test _ =
  let t = Utils.copy_matrix test_matrix_big_box_2 in
  let moved = move_big_box t { y = 4; x = 7 } { dy = -1; dx = 0 } in
  moved
  && equal_char_matrix
       t
       (Utils.char_matrix_of_string
          "##############\n\
           ##......##..##\n\
           ##...[][]...##\n\
           ##....[]....##\n\
           ##..........##\n\
           ##..........##\n\
           ##############")
;;

let try_move_box (b : box) (v : vector) matrix =
  let rec aux (p : point) =
    let n_y, n_x = p.y + v.dy, p.x + v.dx in
    if Utils.coord_outside matrix (n_y, n_x) || Char.equal matrix.(n_y).(n_x) '#'
    then false
    else if Char.equal matrix.(n_y).(n_x) '.'
    then (
      Utils.swap_matrix_elem matrix { y = b.pos.y; x = b.pos.x } { y = n_y; x = n_x };
      true)
    else aux { y = n_y; x = n_x }
  in
  aux b.pos
;;

[@@@ocamlformat "disable"]
let test_matrix = Utils.char_matrix_of_string "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########"
let test_matrix_target = Utils.char_matrix_of_string "########
#...OO.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########"
let test_matrix_2 = Utils.char_matrix_of_string "########
#...OO.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########"
let test_matrix_target_2 = Utils.char_matrix_of_string "########
#....O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#...O..#
########"
[@@@ocamlformat "enable"]

let%test "move one to normal space" =
  let local_test_matrix = Utils.copy_matrix test_matrix in
  let moved =
    try_move_box { pos = { y = 1; x = 3 } } { dy = 0; dx = 1 } local_test_matrix
  in
  moved && equal_char_matrix local_test_matrix test_matrix_target
;;

let%test "move and push others" =
  let local_test_matrix = Utils.copy_matrix test_matrix_2 in
  let moved =
    try_move_box { pos = { y = 1; x = 4 } } { dy = 1; dx = 0 } local_test_matrix
  in
  moved && equal_char_matrix local_test_matrix test_matrix_target_2
;;

let%test "try to move but do nothing" =
  let local_test_matrix = Utils.copy_matrix test_matrix_target_2 in
  let moved =
    try_move_box { pos = { y = 2; x = 4 } } { dy = 1; dx = 0 } local_test_matrix
  in
  (not moved) && equal_char_matrix local_test_matrix local_test_matrix
;;

let find_index matrix element =
  let rows = Array.length matrix in
  let cols = if rows > 0 then Array.length matrix.(0) else 0 in
  let rec loop r c =
    if r >= rows
    then None
    else if c >= cols
    then loop (r + 1) 0
    else if Char.equal matrix.(r).(c) element
    then Some (r, c)
    else loop r (c + 1)
  in
  loop 0 0
;;

let find_robot matrix =
  match find_index matrix '@' with
  | Some (y, x) -> { pos = { y; x } }
  | None -> failwith "robot not found"
;;

let direction_to_vector dir =
  match dir with
  | Left -> { dy = 0; dx = -1 }
  | Top -> { dy = -1; dx = 0 }
  | Right -> { dy = 0; dx = 1 }
  | Bottom -> { dy = 1; dx = 0 }
;;

let move_robot robot matrix vector =
  let n_y, n_x = robot.pos.y + vector.dy, robot.pos.x + vector.dx in
  if Char.equal matrix.(n_y).(n_x) '#'
  then ()
  else if Char.equal matrix.(n_y).(n_x) '.'
  then
    Utils.swap_matrix_elem
      matrix
      { y = robot.pos.y; x = robot.pos.x }
      { y = n_y; x = n_x }
  else if Char.equal matrix.(n_y).(n_x) 'O'
  then (
    let moved = try_move_box { pos = { y = n_y; x = n_x } } vector matrix in
    if moved
    then
      Utils.swap_matrix_elem
        matrix
        { y = robot.pos.y; x = robot.pos.x }
        { y = n_y; x = n_x })
  else if Char.equal matrix.(n_y).(n_x) '[' || Char.equal matrix.(n_y).(n_x) ']'
  then (
    let moved = move_big_box matrix { y = n_y; x = n_x } vector in
    if moved
    then
      Utils.swap_matrix_elem
        matrix
        { y = robot.pos.y; x = robot.pos.x }
        { y = n_y; x = n_x })
;;

let sum_gps matrix =
  Array.foldi matrix ~init:0 ~f:(fun y acc row ->
    Array.foldi row ~init:acc ~f:(fun x acc2 cell ->
      acc2
      +
      match cell with
      | 'O' | '[' -> (100 * y) + x
      | _ -> 0))
;;

let get_gps matrix directions =
  List.iter directions ~f:(fun dir ->
    let robot = find_robot matrix in
    let vec = direction_to_vector dir in
    move_robot robot matrix vec);
  let gps_sum = sum_gps matrix in
  gps_sum
;;

let parse_input str =
  str
  |> Str.split (Str.regexp "\n\n")
  |> (function
        | matrix_str :: dirs_str :: _ -> matrix_str, dirs_str
        | _ -> failwith "bad input")
  |> fun (matrix_str, dirs_str) ->
  let matrix = Utils.char_matrix_of_string matrix_str in
  let directions = parse_directions dirs_str in
  matrix, directions
;;

let make_new_map matrix =
  let new_matrix =
    Array.make_matrix ~dimx:(Array.length matrix) ~dimy:(Array.length matrix.(0) * 2) '-'
  in
  Array.iteri matrix ~f:(fun y row ->
    Array.iteri row ~f:(fun x cell ->
      let i1, i2 =
        match cell with
        | '#' -> '#', '#'
        | '@' -> '@', '.'
        | '.' -> '.', '.'
        | 'O' -> '[', ']'
        | _ -> failwith (sprintf "unknown symbol: %c" cell)
      in
      new_matrix.(y).(2 * x) <- i1;
      new_matrix.(y).((2 * x) + 1) <- i2));
  new_matrix
;;

[@@@ocamlformat "disable"]
let test_matrix_new_map_before = Utils.char_matrix_of_string "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######"
let test_matrix_new_map_after =  Utils.char_matrix_of_string "##############
##......##..##
##..........##
##....[][]@.##
##....[]....##
##..........##
##############"
[@@@ocamlformat "enable"]

let%test _ =
  equal_char_matrix (make_new_map test_matrix_new_map_before) test_matrix_new_map_after
;;

let solve_part1 str =
  let matrix, direction = parse_input str in
  get_gps matrix direction
;;

[@@@ocamlformat "disable"]
let test_input_small = "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"
let test_input_big = "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
[@@@ocamlformat "enable"]

let%test _ = solve_part1 test_input_small = 2028
let%test _ = solve_part1 test_input_big = 10092

let solve_part2 str =
  let matrix, direction = parse_input str in
  let new_matrix = make_new_map matrix in
  get_gps new_matrix direction
;;
