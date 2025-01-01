(* decompose
   1. use bfs to find shortest path
   2. use bfs_with_cheaing where first unused wall is removed and store it after in hashtbl
*)

open Core

type point = int * int [@@deriving sexp, hash, show, equal, compare]

module Point = struct
  type t = point

  let sexp_of_t = sexp_of_point
  let t_of_sexp = point_of_sexp
  let hash = hash_point
  let compare = compare_point
  let ( + ) (y1, x1) (y2, x2) = y1 + y2, x1 + x2
  let sum = ( + )

  let manhattan_distance (y1, x1) (y2, x2) =
    Int.( + ) (Int.abs (y2 - y1)) (Int.abs (x2 - x1))
  ;;
end

let%test_unit _ = [%test_result: point] (Point.sum (1, 2) (-3, -1)) ~expect:(-2, 1)
let dirs = [ 1, 0; -1, 0; 0, 1; 0, -1 ]

let calc_parent_length tbl start =
  let rec aux key length =
    match Hashtbl.find tbl key with
    | Some new_key -> aux new_key (length + 1)
    | None -> length
  in
  aux start 0
;;

let find_shortest_path matrix =
  let start_point = Utils.find_char_pos matrix 'S' in
  let end_point = Utils.find_char_pos matrix 'E' in
  let used = Hash_set.create (module Point) in
  let distance = Hashtbl.create (module Point) in
  let nodes = [ start_point ] in
  Hashtbl.set distance ~key:start_point ~data:0;
  let rec traverse nodes =
    let not_used_nodes = List.filter nodes ~f:(fun n -> not (Hash_set.mem used n)) in
    if List.length not_used_nodes = 0
    then ()
    else (
      let min_node =
        List.min_elt not_used_nodes ~compare:(fun n1 n2 ->
          let d1 = Hashtbl.find_or_add distance n1 ~default:(fun _ -> Int.max_value) in
          let d2 = Hashtbl.find_or_add distance n2 ~default:(fun _ -> Int.max_value) in
          d1 - d2)
        |> Option.value_exn
      in
      let min_node_d = Hashtbl.find_exn distance min_node in
      Hash_set.add used min_node;
      let y, x = min_node in
      let valid_neighbours =
        List.filter_map dirs ~f:(fun (dy, dx) ->
          let ny, nx = y + dy, x + dx in
          if Utils.coord_outside matrix (ny, nx)
          then None
          else if equal_char matrix.(ny).(nx) '#'
          then None
          else Some (ny, nx))
      in
      List.iter valid_neighbours ~f:(fun (ny, nx) ->
        if min_node_d + 1
           < Hashtbl.find_or_add distance (ny, nx) ~default:(fun _ -> Int.max_value)
        then Hashtbl.set distance ~key:(ny, nx) ~data:(min_node_d + 1));
      traverse (nodes @ valid_neighbours))
  in
  traverse nodes;
  let end_point_d = Hashtbl.find_exn distance end_point in
  end_point_d, distance
;;

let cheat_dirs = [ [ -1, 0; 1, 0 ]; [ 0, -1; 0, 1 ] ]

let make_cheats_queue matrix =
  let cheats_queue = Queue.create () in
  Array.iteri matrix ~f:(fun y row ->
    Array.iteri row ~f:(fun x cell ->
      if equal_char cell '#'
      then (
        let cheat_line =
          List.find cheat_dirs ~f:(fun v ->
            List.for_all v ~f:(fun (dy, dx) ->
              (not (Utils.coord_outside matrix (y + dy, x + dx)))
              && (equal_char matrix.(y + dy).(x + dx) '.'
                  || equal_char matrix.(y + dy).(x + dx) 'S'
                  || equal_char matrix.(y + dy).(x + dx) 'E')))
        in
        match cheat_line with
        | Some _ -> Queue.enqueue cheats_queue (y, x)
        | None -> ())
      else ()));
  cheats_queue
;;

let%test_unit _ =
  let matrix = Utils.char_matrix_of_string "###\n.#.\n..#" in
  let cheats_queue = make_cheats_queue matrix in
  let expected_cheats_queue = Queue.create () in
  Queue.enqueue expected_cheats_queue (1, 1);
  [%test_result: point Queue.t] cheats_queue ~expect:expected_cheats_queue
;;

type point_list = point list [@@deriving show]

let calc_path_size_with_cheats matrix =
  (* let points_cache = Hashtbl.create (module Point) in *)
  let fair_shortest_path, distance = find_shortest_path matrix in
  let cheats_queue = make_cheats_queue matrix in
  let rec aux cheats_results =
    if Queue.is_empty cheats_queue
    then cheats_results
    else (
      let cheat = Queue.dequeue_exn cheats_queue in
      let valid_dirs =
        List.filter dirs ~f:(fun vector -> Hashtbl.mem distance (Point.sum cheat vector))
      in
      if List.length valid_dirs < 2
      then aux cheats_results
      else (
        let sorted_neighbours =
          List.map valid_dirs ~f:(fun v -> Point.sum cheat v)
          |> List.sort ~compare:(fun p1 p2 ->
            let p1_d = Hashtbl.find_exn distance p1 in
            let p2_d = Hashtbl.find_exn distance p2 in
            p1_d - p2_d)
        in
        let min_neighbour = List.nth_exn sorted_neighbours 0 in
        let max_neighbour =
          List.nth_exn sorted_neighbours (List.length sorted_neighbours - 1)
        in
        let path_to_cheat = Hashtbl.find_exn distance min_neighbour + 1 in
        let path_from_cheat =
          fair_shortest_path - Hashtbl.find_exn distance max_neighbour + 1
        in
        let shortest_path = path_to_cheat + path_from_cheat in
        aux ((cheat, shortest_path) :: cheats_results)))
  in
  aux [], fair_shortest_path
;;

let find_best_cheats_count matrix =
  let cheats_results, fair_shortest_path = calc_path_size_with_cheats matrix in
  let best_cheats =
    List.filter cheats_results ~f:(fun (_, shortest_path) ->
      fair_shortest_path - shortest_path >= 100)
  in
  List.length best_cheats
;;

let matrix =
  Utils.char_matrix_of_string
    "###############\n\
     #...#...#.....#\n\
     #.#.#.#.#.###.#\n\
     #S#...#.#.#...#\n\
     #######.#.#.###\n\
     #######.#.#...#\n\
     #######.#.###.#\n\
     ###..E#...#...#\n\
     ###.#######.###\n\
     #...###...#...#\n\
     #.#####.#.###.#\n\
     #.#...#.#.#...#\n\
     #.#.#.#.#.#.###\n\
     #...#...#...###\n\
     ###############"
;;

let%test_unit _ =
  let result, _ = find_shortest_path matrix in
  [%test_result: int] result ~expect:84
;;

let find_cheats_with_savings distance =
  let combinations = Utils.make_pair_combinations (Hashtbl.to_alist distance) in
  let rec aux combinations a b =
    match combinations with
    | hd :: tl ->
      let (p1, d1), (p2, d2) = hd in
      let man_dist = Point.manhattan_distance p1 p2 in
      let d = Int.abs (d2 - d1) - man_dist in
      (* printf
         "saving=%d; %s, %d %s, %d = %d\n"
         d
         (show_point p1)
         d1
         (show_point p2)
         d2
         man_dist; *)
      let a = if man_dist = 2 && d >= 100 then a + 1 else a in
      let b = if man_dist < 21 && d >= 100 then b + 1 else b in
      aux tl a b
    | [] -> a, b
  in
  aux combinations 0 0
;;

let solve_part1 str =
  let matrix = Utils.char_matrix_of_string str in
  let _, distance = find_shortest_path matrix in
  let a, _ = find_cheats_with_savings distance in
  a
;;

let solve_part2 str =
  let matrix = Utils.char_matrix_of_string str in
  let _, distance = find_shortest_path matrix in
  let _, b = find_cheats_with_savings distance in
  b
;;
