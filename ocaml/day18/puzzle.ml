(* decompose
   it's dijkstra again, so we can practice, probably we need backtrack in the part 2, but for now we can just go
   also this start with right types, node and edge
*)

open Core

type point =
  { x : int
  ; y : int
  }
[@@deriving equal, make, sexp, compare, hash, show]

type node = { value : point } [@@deriving equal, make, sexp, compare, hash, show]

module Node = struct
  type t = node

  let sexp_of_t = sexp_of_node
  let t_of_sexp = node_of_sexp
  let hash = hash_node
  let compare = compare_node
end

let str_to_point_array str =
  let str_lines = Str.split (Str.regexp "\n") str in
  str_lines
  |> List.map ~f:(Str.split (Str.regexp ","))
  |> List.map ~f:(List.map ~f:int_of_string)
  |> List.map ~f:(fun str_nums ->
    { x = List.nth_exn str_nums 0; y = List.nth_exn str_nums 1 })
  |> Array.of_list
;;

let%test _ =
  let result = str_to_point_array "5,4\n4,2" in
  equal_array equal_point result [| make_point ~y:4 ~x:5; make_point ~y:2 ~x:4 |]
;;

let fill_matrix matrix point_array =
  Array.iter point_array ~f:(fun { y; x } -> matrix.(y).(x) <- '#')
;;

let make_fill_matrix size point_array =
  let matrix = Array.make_matrix ~dimx:size ~dimy:size '.' in
  fill_matrix matrix point_array;
  matrix
;;

type char_matrix = char array array [@@deriving show, equal]

let%test _ =
  let matrix =
    make_fill_matrix
      7
      (str_to_point_array "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n")
  in
  equal_char_matrix
    matrix
    (Utils.char_matrix_of_string
       "...#...\n..#..#.\n....#..\n...#..#\n..#..#.\n.#..#..\n#.#....")
;;

let masks = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

let find_shortest_path ?(exit_on_first_finish = false) matrix exit_point =
  let distance = Hashtbl.create (module Node) in
  let used = Hash_set.create (module Node) in
  let nodes = [ { value = { y = 0; x = 0 } } ] in
  Hashtbl.set distance ~key:(List.hd_exn nodes) ~data:0;
  let rec dijkstra nodes =
    let not_used_nodes =
      List.filter nodes ~f:(fun node -> not (Hash_set.mem used node))
    in
    let min_node =
      List.min_elt not_used_nodes ~compare:(fun n1 n2 ->
        Hashtbl.find_exn distance n1 - Hashtbl.find_exn distance n2)
    in
    match min_node with
    | None -> ()
    | Some min_node ->
      if exit_on_first_finish && equal_point min_node.value exit_point
      then ()
      else (
        let min_node_d = Hashtbl.find_exn distance min_node in
        let valid_neighbours =
          List.filter_map masks ~f:(fun (dy, dx) ->
            let new_point = { y = min_node.value.y + dy; x = min_node.value.x + dx } in
            if Utils.coord_outside matrix (new_point.y, new_point.x)
            then None
            else if equal_char matrix.(new_point.y).(new_point.x) '#'
            then None
            else Some { value = new_point })
        in
        List.iter valid_neighbours ~f:(fun n ->
          let n_d = min_node_d + 1 in
          if n_d < Hashtbl.find_or_add distance n ~default:(fun _ -> Int.max_value)
          then Hashtbl.set distance ~key:n ~data:n_d);
        Hash_set.add used min_node;
        dijkstra (valid_neighbours @ nodes))
  in
  dijkstra nodes;
  let shortest_path_size = Hashtbl.find distance { value = exit_point } in
  (* Hashtbl.iteri distance ~f:(fun ~key:node ~data:d ->
     printf "%s: %d\n" (show_node node) d); *)
  shortest_path_size
;;

let%test _ =
  let matrix =
    make_fill_matrix
      7
      (str_to_point_array "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n")
  in
  equal_option equal_int (find_shortest_path matrix { y = 6; x = 6 }) (Some 22)
;;

let solve_part1 str =
  let size = 71 in
  let point_array = str_to_point_array str in
  let first_kilobyte = Array.slice point_array 0 1024 in
  let matrix = make_fill_matrix size first_kilobyte in
  Option.value_exn (find_shortest_path matrix { y = size - 1; x = size - 1 })
;;

let find_first_blocking_byte size point_array =
  let rec binary_search left right =
    Out_channel.flush stdout;
    Out_channel.flush stderr;
    if left >= right
    then left
    else (
      let mid = left + ((right - left) / 2) in
      let points_slice = Array.slice point_array 0 mid in
      let matrix = make_fill_matrix size points_slice in
      let shortest_path =
        find_shortest_path
          ~exit_on_first_finish:true
          matrix
          { y = size - 1; x = size - 1 }
      in
      match shortest_path with
      | Some _ -> binary_search (mid + 1) right
      | None -> binary_search left (mid - 1))
  in
  let first_block_point_idx = binary_search 0 (Array.length point_array) in
  point_array.(first_block_point_idx)
;;

(* let%test _ = *)
let point_array =
  str_to_point_array
    "5,4\n\
     4,2\n\
     4,5\n\
     3,0\n\
     2,1\n\
     6,3\n\
     2,4\n\
     1,5\n\
     0,6\n\
     3,3\n\
     2,6\n\
     5,1\n\
     1,2\n\
     5,5\n\
     2,5\n\
     6,5\n\
     1,4\n\
     0,4\n\
     6,4\n\
     1,1\n\
     6,1\n\
     1,0\n\
     0,5\n\
     1,6\n\
     2,0"
;;

let _ = find_first_blocking_byte 7 point_array

(* equal_point result { x = 6; y = 1 }
;; *)

let solve_part2 str =
  let point_array = str_to_point_array str in
  let first_blocking_point = find_first_blocking_byte 71 point_array in
  show_point first_blocking_point
;;
