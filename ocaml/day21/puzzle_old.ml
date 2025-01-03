(* decompose
   1. make a function which takes a matrix and target and a search for best moves
*)

open Core

type point =
  { y : int
  ; x : int
  }
[@@deriving sexp, show, hash, compare, equal]

module Point = struct
  type t = point

  let sexp_of_t = sexp_of_point
  let t_of_sexp = point_of_sexp
  let hash = hash_point
  let compare = compare_point
  let ( + ) p1 p2 = { y = p1.y + p2.y; x = p1.x + p2.x }
  let ( - ) p1 p2 = { y = p1.y - p2.y; x = p1.x - p2.x }
  let sum = ( + )

  let man_dist p1 p2 =
    Int.( + ) (Int.abs (Int.( - ) p2.y p1.y)) (Int.abs (Int.( - ) p2.x p1.x))
  ;;

  let outside matrix p = Utils.coord_outside matrix (p.y, p.x)
  let of_pair (y, x) = { y; x }
end

let find_char_pos matrix ch = Point.of_pair (Utils.find_char_pos matrix ch)

let str_to_point_list matrix str =
  let char_list = String.to_list str in
  List.map char_list ~f:(find_char_pos matrix)
;;

let vectors = [
  { y = 1; x = 0};
  { y = -1; x = 0};
  { y = 0; x = -1};
  { y = 0; x = 1};
] [@@ocamlformat "disable"]

let numpad_matrix = Utils.char_matrix_of_string "789\n456\n123\nx0A"
let dir_matrix = Utils.char_matrix_of_string "x^A\n<v>"

let find_best_neigbhours ?(vectors = vectors) matrix p1 p2 avoid_list =
  let valid_neigbhours =
    List.filter_map vectors ~f:(fun vec ->
      let np = Point.sum p1 vec in
      if Point.outside matrix np
      then None
      else if List.mem avoid_list np ~equal:equal_point
      then None
      else Some np)
  in
  (* printf "%s %s\n\n" (show_point p1) (show_point p2); *)
  (* List.iter valid_neigbhours ~f:(fun p -> printf "%s\n" (show_point p)); *)
  let neighbours_distances =
    List.map valid_neigbhours ~f:(fun np -> np, Point.man_dist np p2)
  in
  let min_neighbour =
    List.min_elt neighbours_distances ~compare:(fun n1 n2 -> snd n1 - snd n2)
    |> Option.value_exn
  in
  let all_min_neighbours =
    List.filter neighbours_distances ~f:(fun (_, d) -> d = snd min_neighbour)
  in
  List.map all_min_neighbours ~f:fst
;;

let%test_unit _ =
  [%test_result: point list]
    (find_best_neigbhours
       numpad_matrix
       (find_char_pos numpad_matrix '0')
       (find_char_pos numpad_matrix '7')
       [ find_char_pos numpad_matrix 'x' ])
    ~expect:[ find_char_pos numpad_matrix '2' ]
;;

let%test_unit _ =
  [%test_result: point list]
    (find_best_neigbhours
       numpad_matrix
       (find_char_pos numpad_matrix '8')
       (find_char_pos numpad_matrix '1')
       [ find_char_pos numpad_matrix 'x' ])
    ~expect:[ find_char_pos numpad_matrix '5'; find_char_pos numpad_matrix '7' ]
;;

let%test_unit _ =
  [%test_result: point list]
    (find_best_neigbhours
       dir_matrix
       (find_char_pos dir_matrix '^')
       (find_char_pos dir_matrix '<')
       [ find_char_pos dir_matrix 'x' ])
    ~expect:[ find_char_pos dir_matrix 'v' ]
;;

let get_paths ?(vectors = vectors) matrix p1 p2 avoid_list =
  let confirm_pos = find_char_pos matrix 'A' in
  let rec aux paths =
    let new_paths =
      List.fold paths ~init:[] ~f:(fun acc path ->
        match path with
        | hd :: tl ->
          let best_neighbours = find_best_neigbhours matrix hd p2 avoid_list ~vectors in
          let new_paths = List.map best_neighbours ~f:(fun n -> n :: hd :: tl) in
          new_paths @ acc
        | _ -> acc)
    in
    if equal_point p2 (List.hd_exn (List.hd_exn paths)) then new_paths else aux new_paths
  in
  let paths = aux [ [ p1 ] ] in
  let paths = List.map paths ~f:(fun p -> confirm_pos :: p) in
  paths |> List.map ~f:List.rev
;;

(* let%test_unit _ =
   [%test_result: point list]
   (get_path
   numpad_matrix
   (find_char_pos numpad_matrix '0')
   (find_char_pos numpad_matrix '7')
   [ find_char_pos numpad_matrix 'x' ])
   ~expect:
   [ find_char_pos numpad_matrix '0'
      ; find_char_pos numpad_matrix '2'
      ; find_char_pos numpad_matrix '5'
      ; find_char_pos numpad_matrix '8'
      ; find_char_pos numpad_matrix '7'
      ]
   ;; *)

type button =
  { vector : point
  ; name : char
  }

let confirm_button = { vector = { y = 0; x = 0 }; name = 'A' }

let buttons =
  [ { vector = { y = 0; x = -1 }; name = '<' }
  ; { vector = { y = -1; x = 0 }; name = '^' }
  ; { vector = { y = 0; x = 1 }; name = '>' }
  ; { vector = { y = 1; x = 0 }; name = 'v' }
  ; confirm_button
  ]
;;

let path_to_buttons path =
  let rec aux path =
    match path with
    | p1 :: p2 :: tl ->
      let vector = Point.( - ) p2 p1 in
      let button = List.find_exn buttons ~f:(fun b -> equal_point b.vector vector) in
      button :: aux (p2 :: tl)
    | _ -> []
  in
  aux path
;;

let shortest_paths_full ?(vectors = vectors) matrix target_list avoid_list start_point =
  (* printf "start shortest\n"; *)
  let rec aux cur_point target_list =
    match target_list with
    | target :: tl ->
      let paths = get_paths matrix cur_point target avoid_list ~vectors in
      List.map paths ~f:path_to_buttons :: aux target tl
    | [] -> []
  in
  let buttons = aux start_point target_list in
  List.map buttons ~f:(fun bll -> List.map bll ~f:(fun bl -> List.map bl ~f:(fun b -> b.name)))
;;

let shortest_path ?(vectors = vectors) matrix target_list =
  shortest_paths_full
    matrix
    target_list
    (str_to_point_list matrix "x")
    (find_char_pos matrix 'A')
    ~vectors
;;

(*
   let%test_unit _ =
   [%test_result: char list]
   (shortest_path numpad_matrix (str_to_point_list numpad_matrix "029A"))
   ~expect:(String.to_list "<A^A^^>AvvvA")
   ;;

   let%test_unit _ =
   [%test_result: char list]
   (shortest_path dir_matrix (str_to_point_list dir_matrix "<"))
   ~expect:(String.to_list "v<<A")
   ;;

   let%test_unit _ =
   [%test_result: char list]
   (shortest_path dir_matrix (str_to_point_list dir_matrix "<A^A>^^AvvvA"))
   ~expect:(String.to_list "v<<A>^>A<A>AvA^<AA>Av<AAA^>A")
   ;;

   let%test_unit _ =
   [%test_result: char list]
   (shortest_path
   ~vectors:
   [ { y = 0; x = -1 }; { y = 0; x = 1 }; { y = 1; x = 0 }; { y = -1; x = 0 } ]
   dir_matrix
   (str_to_point_list dir_matrix "v<<A>^>A<A>AvA^<AA>Av<AAA^>A"))
   ~expect:
   (String.to_list
   "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")
   ;; *)

(* let fast code =
   let code_list = String.to_list code in
   let q = Queue.of_list code_list in
   let rec aux p1 p2 p3 =
   if Queue.is_empty q
   then ()
   else (
   let char = Queue.dequeue_exn q in
   let rec aux2 p1 p2 p3 =
   let p1_ns =
   find_best_neigbhours
   numpad_matrix
   p1
   (find_char_pos numpad_matrix char)
   [ find_char_pos numpad_matrix 'x' ]
   in

   p1_ns
   in
   aux2 p1 p2 p3)
   in
   aux _ _ _
   ;; *)

let find_first_min_idx lst =
  let min = List.min_elt lst ~compare:compare_int 
  |> Option.value_exn in
  let _, idx =List.findi_exn lst ~f:(fun _ value -> value = min) in
  idx

let solve_part1 str =
  let codes = Str.split (Str.regexp "\n") str in
  let codes_complexity =
    List.map codes ~f:(fun code ->
      let code_num = String.substr_replace_first code ~pattern:"A" ~with_:"" in
      let code_num = int_of_string code_num in
      let step_1_candidates = shortest_path numpad_matrix (str_to_point_list numpad_matrix code) in

      List.map step_1_candidates ~f:(fun cc1 -> 
        List.map cc1 ~f:(fun c1 ->
          let c1_parents = shortest_path
          dir_matrix
          (str_to_point_list dir_matrix (String.of_char_list c1)) in

          List.map c1_parents ~f:(fun cc2 ->
              List.map cc2 ~f:(fun c2 ->
                let c2_parents = shortest_path
                dir_matrix
                (str_to_point_list dir_matrix (String.of_char_list c2)) in

                let c2_parents_lengths = List.map c2_parents ~f:(fun cc3 ->
                    List.map cc3 ~f:(fun c3 -> List.length c3)
                  ) in
                  List.map2 c2_parents c2_parents_lengths ~f:(fun cc3 cc3_lengths ->
                    
                    )
                )
            )

          )
        )

      (* let instr2 =
        shortest_path
          dir_matrix
          (str_to_point_list dir_matrix (String.of_char_list instr1))
      in
      let instr3 =
        shortest_path
          dir_matrix
          (str_to_point_list dir_matrix (String.of_char_list instr2))
      in
      printf "%d\n" (List.length instr3); *)
      (* List.length instr3 * code_num *)
      )
  in
  List.fold codes_complexity ~init:0 ~f:( + )
;;

(* let%test_unit _ =
   [%test_result: int] (solve_part1 "029A\n980A\n179A\n456A\n379A") ~expect:126384
   ;; *)

let solve_part2 _ = -1
