(* decompose
   for sure we need separate function for calculation UPD. no :D

   the idea is a tree, we check every possible move, we have 3 of them (rotate right, rotate left, step forward)
   there is an edge case when you look down and it doesnt matter if we can turn right or left
   so I think first step is calc open directions [left, top, right, bottom]
   if left is open and we look left go step forward, otherwise find shortest turn path
   and the go forward (or add to queue to check price of this path)

   tail optimization is way found, check that others price is no more the min price for now

   also we also need prioritize min_turns direction, if we look left and open ways are top and right
   we should start with top

   so what kind of functions do we need
   find_cheapest_path start_point end_point matrix
*)

open Core

type point =
  { y : int
  ; x : int
  }
[@@deriving show, equal, sexp, compare, hash]

type point_list = point list [@@deriving equal]

type vector =
  { dy : int
  ; dx : int
  }

type direction =
  | Left
  | Top
  | Right
  | Bottom
[@@deriving show, equal, sexp, compare, hash]

type reindeer =
  { dir : direction
  ; pos : point
  }
[@@deriving show, equal, sexp, compare, hash]

type reindeer_list = reindeer list [@@deriving show]

let get_dir_vector d =
  match d with
  | Left -> { dy = 0; dx = -1 }
  | Top -> { dy = -1; dx = 0 }
  | Right -> { dy = 0; dx = 1 }
  | Bottom -> { dy = 1; dx = 0 }
;;

let get_inv_dir_vector d =
  match d with
  | Left -> { dy = 0; dx = 1 }
  | Top -> { dy = 1; dx = 0 }
  | Right -> { dy = 0; dx = -1 }
  | Bottom -> { dy = -1; dx = 0 }
;;

let turn_right = function
  | Left -> Top
  | Top -> Right
  | Right -> Bottom
  | Bottom -> Left
;;

let turn_left = function
  | Left -> Bottom
  | Bottom -> Right
  | Right -> Top
  | Top -> Left
;;

let turn_twice = function
  | Left -> Right
  | Top -> Bottom
  | Right -> Left
  | Bottom -> Top
;;

type char_matrix = char array array [@@deriving show]

let i = ref 0

module Reindeer = struct
  type t = reindeer

  let compare = compare_reindeer
  let sexp_of_t = sexp_of_reindeer
  let t_of_sexp = reindeer_of_sexp
  let hash = hash_reindeer
end

module Point = struct
  type t = point

  let compare = compare_point
  let sexp_of_t = sexp_of_point
  let t_of_sexp = point_of_sexp
  let hash = hash_point
end

module ReindeerSet = Set.Make (Reindeer)
module PointSet = Set.Make (Point)

type graph_edge =
  { r : reindeer
  ; distance : int
  }

type graph_node = { r : reindeer } [@@deriving show, equal, compare, sexp, hash]

module GraphNode = struct
  type t = graph_node

  let sexp_of_t = sexp_of_graph_node
  let t_of_sexp = graph_node_of_sexp
  let compare = compare_graph_node
  let hash = hash_graph_node
end

let print_path_list path matrix =
  let m = Utils.copy_matrix matrix in
  List.iter path ~f:(fun r -> m.(r.y).(r.x) <- 'O');
  printf "%s\n\n" (show_char_matrix m)
;;

let rec get_node_paths r tbl : reindeer list list =
  let parents_opt = Hashtbl.find tbl r in
  match parents_opt with
  | Some parents ->
    if List.length parents = 0
    then [ [ r ] ]
    else
      List.fold parents ~init:[] ~f:(fun acc parent ->
        let parent_paths = get_node_paths parent tbl in
        let new_paths = List.map parent_paths ~f:(fun parent_path -> r :: parent_path) in
        acc @ new_paths)
  | None -> [ [ r ] ]
;;

let dijkstra r matrix =
  let table = Hashtbl.create (module Reindeer) in
  let used_set = Hash_set.create (module Reindeer) in
  let path_tiles_set = Hash_set.create (module Reindeer) in
  Hashtbl.set table ~key:r ~data:0;
  let vertices : graph_node list = [ { r } ] in
  let rec aux vertices =
    let not_used_v = List.filter vertices ~f:(fun v -> not (Hash_set.mem used_set v.r)) in
    if List.length not_used_v = 0
    then ()
    else (
      let min_n =
        List.fold not_used_v ~init:(List.nth_exn not_used_v 0) ~f:(fun min_v v ->
          if Hash_set.mem used_set v.r
          then min_v
          else (
            let cur_d = Hashtbl.find_exn table v.r in
            let min_d = Hashtbl.find_exn table min_v.r in
            if cur_d < min_d then v else min_v))
      in
      (* print_path (Some min_n) matrix; *)
      let min_v = min_n.r in
      (* print_path (Some min_n) matrix; *)
      Hash_set.add used_set min_v;
      let min_d = Hashtbl.find_exn table min_v in
      (* printf "%s %d\n" (show_reindeer min_v) min_d; *)
      let dir_list = [ min_v.dir; turn_right min_v.dir; turn_left min_v.dir ] in
      let new_vertices, new_edges =
        List.foldi dir_list ~init:([], []) ~f:(fun i (vertices, (edges : int list)) dir ->
          let v = get_dir_vector dir in
          let new_p = { y = min_v.pos.y + v.dy; x = min_v.pos.x + v.dx } in
          let new_r = { pos = new_p; dir } in
          let new_n = { r = new_r } in
          if equal_char matrix.(new_p.y).(new_p.x) '#'
          then vertices, edges
          else (
            let turn_cost =
              match i with
              | 0 -> 0
              | 1 | 2 -> 1000
              | _ -> failwith "unexpected turns count"
            in
            new_n :: vertices, (1 + turn_cost) :: edges))
      in
      List.iter2_exn new_vertices new_edges ~f:(fun v e ->
        if min_d + e < Hashtbl.find_or_add table v.r ~default:(fun () -> Int.max_value)
        then Hashtbl.set table ~key:v.r ~data:(min_d + e));
      aux (vertices @ new_vertices))
  in
  aux vertices;
  let rec backtrack r =
    Hash_set.add path_tiles_set r;
    let dir_list = [ r.dir; turn_right r.dir; turn_left r.dir ] in
    let new_vertices, new_edges =
      List.fold dir_list ~init:([], []) ~f:(fun (vertices, (edges : int list)) dir ->
        let v = get_inv_dir_vector dir in
        let vars =
          [ { pos = { y = r.pos.y + v.dy; x = r.pos.x + v.dx }; dir = r.dir }, 1
          ; ( { pos = { y = r.pos.y + v.dy; x = r.pos.x + v.dx }; dir = turn_right r.dir }
            , 1001 )
          ; ( { pos = { y = r.pos.y + v.dy; x = r.pos.x + v.dx }; dir = turn_left r.dir }
            , 1001 )
          ; ( { pos = { y = r.pos.y + v.dy; x = r.pos.x + v.dx }; dir = turn_right r.dir }
            , 1 )
          ; { pos = { y = r.pos.y + v.dy; x = r.pos.x + v.dx }; dir = turn_left r.dir }, 1
          ]
        in
        let new_r_list, new_c_list =
          List.fold vars ~init:([], []) ~f:(fun (r_list, c_list) (r, c) ->
            r :: r_list, c :: c_list)
        in
        new_r_list @ vertices, new_c_list @ edges)
    in
    let r_price = Hashtbl.find_exn table r in
    let used_2_set = Hash_set.create (module Reindeer) in
    List.iter2_exn new_vertices new_edges ~f:(fun v e ->
      if (not (Hash_set.mem used_2_set v))
         && r_price - e = Hashtbl.find_or_add table v ~default:(fun () -> Int.max_value)
      then (
        Hash_set.add used_2_set v;
        backtrack v))
  in
  (* Hashtbl.iteri parents ~f:(fun ~key:r ~data:parents ->
     printf "r: %s | parents: %d\n" (show_reindeer r) (List.length parents)); *)
  let end_nodes =
    Hashtbl.fold table ~init:[] ~f:(fun ~key ~data:_ acc ->
      if equal_char matrix.(key.pos.y).(key.pos.x) 'E' then key :: acc else acc)
  in
  let end_nodes, _ =
    List.fold
      end_nodes
      ~init:([], Int.max_value)
      ~f:(fun ((result_nodes : reindeer_list), min_cost) key ->
        let cur_min = Hashtbl.find_or_add table key ~default:(fun () -> Int.max_value) in
        if cur_min < min_cost
        then [ key ], cur_min
        else if cur_min = min_cost
        then key :: result_nodes, min_cost
        else result_nodes, min_cost)
  in
  (* printf "nodes: %s\n" (show_reindeer_list end_nodes); *)
  backtrack (List.hd_exn end_nodes);
  let unique_rs = Hash_set.to_list path_tiles_set in
  let unique_cells = List.map unique_rs ~f:(fun r -> r.pos) in
  let unique_cells_set = Hash_set.of_list (module Point) unique_cells in
  let unique_cells = Hash_set.to_list unique_cells_set in
  (* print_path_list unique_cells matrix; *)
  let cheapest_path_cost =
    Hashtbl.fold table ~init:Int.max_value ~f:(fun ~key ~data min_cost ->
      if equal_char matrix.(key.pos.y).(key.pos.x) 'E'
      then if data < min_cost then data else min_cost
      else min_cost)
  in
  cheapest_path_cost, List.length unique_cells
;;

let find_reindeer matrix =
  let p_opt =
    Array.find_mapi matrix ~f:(fun y row ->
      Array.find_mapi row ~f:(fun x cell ->
        if equal_char cell 'S' then Some { y; x } else None))
  in
  let p = Option.value_exn p_opt in
  { pos = p; dir = Right }
;;

let solve_part1 str =
  let matrix = Utils.char_matrix_of_string str in
  let reindeer = find_reindeer matrix in
  let shortest_path_length, _ = dijkstra reindeer matrix in
  shortest_path_length
;;

let%test _ =
  let r =
    solve_part1
      "###############\n\
       #.......#....E#\n\
       #.#.###.#.###.#\n\
       #.....#.#...#.#\n\
       #.###.#####.#.#\n\
       #.#.#.......#.#\n\
       #.#.#####.###.#\n\
       #...........#.#\n\
       ###.#.#####.#.#\n\
       #...#.....#.#.#\n\
       #.#.#.###.#.#.#\n\
       #.....#...#.#.#\n\
       #.###.#.#.#.#.#\n\
       #S..#.....#...#\n\
       ###############"
  in
  printf "result: %d\n" r;
  r = 7036
;;

let%test _ =
  let r =
    solve_part1
      "#################\n\
       #...#...#...#..E#\n\
       #.#.#.#.#.#.#.#.#\n\
       #.#.#.#...#...#.#\n\
       #.#.#.#.###.#.#.#\n\
       #...#.#.#.....#.#\n\
       #.#.#.#.#.#####.#\n\
       #.#...#.#.#.....#\n\
       #.#.#####.#.###.#\n\
       #.#.#.......#...#\n\
       #.#.###.#####.###\n\
       #.#.#...#.....#.#\n\
       #.#.#.#####.###.#\n\
       #.#.#.........#.#\n\
       #.#.#.#########.#\n\
       #S#.............#\n\
       #################"
  in
  printf "result: %d\n" r;
  r = 11048
;;

let solve_part2 str =
  let matrix = Utils.char_matrix_of_string str in
  let reindeer = find_reindeer matrix in
  let _, unique_nodes_cnt = dijkstra reindeer matrix in
  unique_nodes_cnt
;;

let%test _ =
  let r =
    solve_part2
      "###############\n\
       #.......#....E#\n\
       #.#.###.#.###.#\n\
       #.....#.#...#.#\n\
       #.###.#####.#.#\n\
       #.#.#.......#.#\n\
       #.#.#####.###.#\n\
       #...........#.#\n\
       ###.#.#####.#.#\n\
       #...#.....#.#.#\n\
       #.#.#.###.#.#.#\n\
       #.....#...#.#.#\n\
       #.###.#.#.#.#.#\n\
       #S..#.....#...#\n\
       ###############"
  in
  printf "result: %d\n" r;
  r = 45
;;

let%test _ =
  let r =
    solve_part2
      "#################\n\
       #...#...#...#..E#\n\
       #.#.#.#.#.#.#.#.#\n\
       #.#.#.#...#...#.#\n\
       #.#.#.#.###.#.#.#\n\
       #...#.#.#.....#.#\n\
       #.#.#.#.#.#####.#\n\
       #.#...#.#.#.....#\n\
       #.#.#####.#.###.#\n\
       #.#.#.......#...#\n\
       #.#.###.#####.###\n\
       #.#.#...#.....#.#\n\
       #.#.#.#####.###.#\n\
       #.#.#.........#.#\n\
       #.#.#.#########.#\n\
       #S#.............#\n\
       #################"
  in
  printf "result: %d\n" r;
  r = 64
;;
