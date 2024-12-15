(* decompose
   we need to predict next position
   calculate 100 times probably ineffective
   actually i think we can calculate only velocity_y * times % rows and put it into matrix
   1. predict_next_position start_point velocity
   2. matrix_map to track count
   3. iterate over every robot and put predict to map
   4. calc_quadrants except midle (horizontal and vertical) lines
   5. multiple them together

   + UPD. 0. - parse input)) because when you come up with the decision
     you don't really want to spend time on parsing
*)
open Core

type point =
  { x : int
  ; y : int
  }
[@@deriving make, show, equal]

type velocity =
  { vx : int
  ; vy : int
  }
[@@deriving make, show, equal]

type robot =
  { start_point : point
  ; mutable current_point : point
  ; velocity : velocity
  }
[@@deriving make, show, equal]

let parse_input_entry str : robot =
  let regexp = Re.compile (Re.Perl.re "-?\\d+") in
  let groups_list = Re.all regexp str in
  let robots_numbers =
    List.map groups_list ~f:(fun group -> int_of_string (Re.Group.get group 0))
  in
  match robots_numbers with
  | x :: y :: vx :: vy :: _ ->
    { start_point = { x; y }; current_point = { x; y }; velocity = { vx; vy } }
  | _ -> failwith "bad numbers"
;;

let%test _ =
  equal_robot
    (parse_input_entry "p=90,28 v=-17,-10")
    (make_robot
       ~start_point:{ x = 90; y = 28 }
       ~current_point:{ x = 90; y = 28 }
       ~velocity:{ vx = -17; vy = -10 })
;;

let parse_input str = str |> String.split_lines |> List.map ~f:parse_input_entry

let predict_robot_position robot n (rows, columns) =
  let new_y = (robot.start_point.y + (robot.velocity.vy * n)) % rows in
  let new_x = (robot.start_point.x + (robot.velocity.vx * n)) % columns in
  { x = new_x; y = new_y }
;;

let%test _ =
  equal_point
    (predict_robot_position
       { start_point = { x = 2; y = 4 }
       ; current_point = { x = 2; y = 4 }
       ; velocity = { vx = 2; vy = -3 }
       }
       5
       (7, 11))
    { x = 1; y = 3 }
;;

let get_quadrant matrix y x =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  let middle_y_idx = rows / 2 in
  let middle_x_idx = columns / 2 in
  if y < middle_y_idx && x < middle_x_idx
  then 0
  else if y < middle_y_idx && x > middle_x_idx
  then 1
  else if y > middle_y_idx && x < middle_x_idx
  then 2
  else if y > middle_y_idx && x > middle_x_idx
  then 3
  else -1
;;

let test_matrix = Array.make_matrix ~dimx:5 ~dimy:5 0
let%test _ = get_quadrant test_matrix 1 1 = 0
let%test _ = get_quadrant test_matrix 1 4 = 1
let%test _ = get_quadrant test_matrix 4 1 = 2
let%test _ = get_quadrant test_matrix 4 4 = 3
let%test "middle rows are not quadrants" = get_quadrant test_matrix 2 1 = -1

let get_quadrants_sum matrix =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  let rec aux y x q_sum =
    if y = rows
    then q_sum
    else if x = columns
    then aux (y + 1) 0 q_sum
    else if matrix.(y).(x) = 0
    then aux y (x + 1) q_sum
    else (
      let q1, q2, q3, q4 = q_sum in
      let robots_count_on_point = matrix.(y).(x) in
      let new_q_sum =
        match get_quadrant matrix y x with
        | 0 -> q1 + robots_count_on_point, q2, q3, q4
        | 1 -> q1, q2 + robots_count_on_point, q3, q4
        | 2 -> q1, q2, q3 + robots_count_on_point, q4
        | 3 -> q1, q2, q3, q4 + robots_count_on_point
        | _ -> q1, q2, q3, q4
      in
      aux y (x + 1) new_q_sum)
  in
  aux 0 0 (0, 0, 0, 0)
;;

let%test _ =
  let m = Utils.copy_matrix test_matrix in
  m.(0).(0) <- 1;
  m.(4).(4) <- 1;
  [%equal: int * int * int * int] (get_quadrants_sum m) (1, 0, 0, 1)
;;

(* part 2 decompose
   we have directions to build a christmas tree
   1. top (1, 0) until top is finished, if next top is last or emptyo go next
   2. left (0, -1) go until next empty or outside, after go next
   3. top_right (1, 1) go

   oh my god, we can iterate in fact and see the map
*)

(* let is_christmas_tree = *)

type char_matrix = char array array [@@deriving show]

let print_robots ?(map_size = 103, 101) robots =
  let rows, columns = map_size in
  let robots_map = Array.make_matrix ~dimx:rows ~dimy:columns ' ' in
  List.iter robots ~f:(fun robot ->
    robots_map.(robot.current_point.y).(robot.current_point.x) <- '1');
  Array.iter robots_map ~f:(fun row ->
    Array.iter row ~f:(fun elem -> Printf.printf "%c" elem);
    Stdio.printf "\n");
  Stdio.printf "\n\n-----------\n\n"
;;

(* Example: Generate a huge image *)

(* Convert PPM to PNG: `convert huge_image.ppm huge_image.png` *)

let print_robots_image ?(map_size = 103, 101) robots =
  let rows, columns = map_size in
  let robots_map = Array.make_matrix ~dimx:rows ~dimy:columns 0 in
  List.iter robots ~f:(fun robot ->
    robots_map.(robot.current_point.y).(robot.current_point.x) <- 1);
  (* Array.iter robots_map ~f:(fun row ->
     Array.iter row ~f:(fun elem -> Printf.printf "%d " elem);
     Stdio.printf "\n");
     Stdio.printf "\n\n-----------\n\n" *)
  robots_map
;;

let iterate_and_print_robots robots =
  let matrices = Array.create ~len:10000 (Array.make_matrix ~dimx:1 ~dimy:1 0) in
  let rec aux i =
    if i = 10000
    then ()
    else (
      (* Stdio.printf "i: %d\n" i; *)
      (* if i % 50 < 10 || i % 50 > 40 then print_robots robots; *)
      let robots_map = print_robots_image robots in
      List.iter robots ~f:(fun robot ->
        robot.current_point
        <- { y = (robot.current_point.y + robot.velocity.vy) % 103
           ; x = (robot.current_point.x + robot.velocity.vx) % 101
           });
      matrices.(i) <- robots_map;
      aux (i + 1))
  in
  aux 0;
  Utils.write_huge_image "matrices.ppm" matrices 5
;;

let solve_part1 ?(n = 100) ?(size = 103, 101) str =
  let rows, columns = size in
  let robots = parse_input str in
  iterate_and_print_robots robots;
  let robots_map = Array.make_matrix ~dimx:rows ~dimy:columns 0 in
  List.iter robots ~f:(fun robot ->
    let { y; x } = predict_robot_position robot n (rows, columns) in
    robots_map.(y).(x) <- robots_map.(y).(x) + 1);
  let q1, q2, q3, q4 = get_quadrants_sum robots_map in
  q1 * q2 * q3 * q4
;;

(* let print_robots_iterations robots matrix =
   let robots_map = Array.make_matrix ~dimx:103 ~dimy:101 0 in

   (* let aux n =
   if n = 0 then ()
   else *) *)

let%test "solve example" =
  solve_part1
    ~size:(7, 11)
    "p=0,4 v=3,-3\n\
     p=6,3 v=-1,-3\n\
     p=10,3 v=-1,2\n\
     p=2,0 v=2,-1\n\
     p=0,0 v=1,3\n\
     p=3,0 v=-2,-2\n\
     p=7,6 v=-1,-3\n\
     p=3,0 v=-1,-2\n\
     p=9,3 v=2,3\n\
     p=7,3 v=-1,2\n\
     p=2,4 v=2,-3\n\
     p=9,5 v=-3,-3"
  = 12
;;

(* solved visually, check file matrices.ppm and convert to pdf or image and you find it *)
let solve_part2 _ = -1
