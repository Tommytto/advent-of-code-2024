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
end

let dirs = [ 1, 0; -1, 0; 0, 1; 0, -1 ]

let calc_parent_length tbl start =
  let rec aux key length =
    match Hashtbl.find tbl key with
    | Some new_key -> aux new_key (length + 1)
    | None -> length
  in
  aux start 0
;;

let find_shortest_path matrix points_cache cheat_point max_path =
  let start_point = Utils.find_char_pos matrix 'S' in
  let end_point = Utils.find_char_pos matrix 'E' in
  let q = Queue.create () in
  let parent = Hashtbl.create (module Point) in
  let visited = Hash_set.create (module Point) in
  Queue.enqueue q start_point;
  Hash_set.add visited start_point;
  let rec traverse cheat_point_visited =
    if Queue.is_empty q
    then -1
    else (
      let node = Queue.dequeue_exn q in
      let path_size = calc_parent_length parent node in
      if path_size > max_path
      then -1
      else if (* if cheat_point_visited && Hashtbl.mem points_cache node
                 then calc_parent_length parent node + Hashtbl.find_exn points_cache node *)
              false
      then -1
      else (
        let y, x = node in
        if equal_point node end_point
        then calc_parent_length parent node
        else (
          let neighbours =
            List.filter_map dirs ~f:(fun (dy, dx) ->
              let ny, nx = y + dy, x + dx in
              if Utils.coord_outside matrix (ny, nx)
              then None
              else if equal_char matrix.(ny).(nx) '#'
              then None
              else if Hash_set.mem visited (ny, nx)
              then None
              else Some (ny, nx))
          in
          List.iter neighbours ~f:(fun n ->
            Queue.enqueue q n;
            Hashtbl.set parent ~key:n ~data:node;
            Hash_set.add visited n);
          let cheat_point_visited = cheat_point_visited || equal_point node cheat_point in
          let total_path_size_to_end = traverse cheat_point_visited in
          let path_size_to_end_from_current =
            total_path_size_to_end - calc_parent_length parent node
          in
          (* fill only without cheats *)
          if equal_point cheat_point (-1, -1)
          then Hashtbl.set points_cache ~key:node ~data:path_size_to_end_from_current;
          total_path_size_to_end)))
  in
  traverse false
;;

let find_shortest_path_dijkstra matrix =
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
  end_point_d
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
              && equal_char matrix.(y + dy).(x + dx) '.'))
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

let calc_path_size_with_cheats matrix =
  let points_cache = Hashtbl.create (module Point) in
  let fair_shortest_path = find_shortest_path matrix points_cache (-1, -1) 9999999 in
  let cheats_queue = make_cheats_queue matrix in
  let rec aux cheats_results =
    if Queue.is_empty cheats_queue
    then cheats_results
    else (
      let cheat = Queue.dequeue_exn cheats_queue in
      let cheat_y, cheat_x = cheat in
      matrix.(cheat_y).(cheat_x) <- '.';
      let shortest_path =
        find_shortest_path matrix points_cache cheat (fair_shortest_path - 99)
      in
      matrix.(cheat_y).(cheat_x) <- '#';
      aux ((cheat, shortest_path) :: cheats_results))
  in
  aux [], fair_shortest_path
;;

let find_best_cheats_count matrix =
  let cheats_results, fair_shortest_path = calc_path_size_with_cheats matrix in
  let best_cheats =
    List.filter cheats_results ~f:(fun (_, shortest_path) ->
      fair_shortest_path - shortest_path > 100)
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
  let result = find_shortest_path matrix (Hashtbl.create (module Point)) (-1, -1) 9999 in
  [%test_result: int] result ~expect:84
;;

let solve_part1 str =
  let matrix = Utils.char_matrix_of_string str in
  find_best_cheats_count matrix
;;

let solve_part2 _ = -1
(* let matrix = Utils.char_matrix_of_string str in
   find_best_cheats_count matrix *)
