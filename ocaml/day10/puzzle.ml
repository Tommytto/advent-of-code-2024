(* decompose
   iterate matrix, when find zero, for every +1 neighbour try to find highest point

   rec matrix (curren)
*)

let dirs_masks = [ 0, -1; -1, 0; 0, 1; 1, 0 ]

let next_neighours_points matrix (x, y) =
  let start_point_value = matrix.(x).(y) in
  dirs_masks
  |> List.filter (fun (x_step, y_step) ->
    not (Utils.coord_outside matrix (x + x_step, y + y_step)))
  |> List.filter (fun (x_step, y_step) ->
    matrix.(x + x_step).(y + y_step) = start_point_value + 1)
  |> List.map (fun (x_step, y_step) -> x + x_step, y + y_step)
;;

[@@@ocamlformat "disable"]
let test_matrix = [|
       [|0; 1; 2; 3|];
       [|1; 2; 3; 4|];
       [|8; 7; 6; 5|];
       [|9; 8; 9; 6|]
     |]
[@@@ocamlformat "enable"]

let%test _ = next_neighours_points test_matrix (0, 0) = [ 0, 1; 1, 0 ]

module StringSet = Set.Make (String)

let highest_points_list matrix start_point =
  let rec aux point result =
    let neighbours = next_neighours_points matrix point in
    List.fold_left
      (fun acc (n_x, n_y) ->
        if matrix.(n_x).(n_y) = 9
        then Printf.sprintf "%d_%d" n_x n_y :: acc
        else aux (n_x, n_y) acc)
      result
      neighbours
  in
  let points_list = aux start_point [] in
  points_list
  |> StringSet.of_list
  |> StringSet.to_list
  |> List.map (Str.split (Str.regexp "_"))
  |> List.map (fun lst ->
    match lst with
    | x :: y :: _ -> int_of_string x, int_of_string y
    | _ -> failwith "Bad")
;;

let highest_points_list_all matrix start_point =
  let rec aux point result =
    let neighbours = next_neighours_points matrix point in
    List.fold_left
      (fun acc (n_x, n_y) ->
        if matrix.(n_x).(n_y) = 9
        then Printf.sprintf "%d_%d" n_x n_y :: acc
        else aux (n_x, n_y) acc)
      result
      neighbours
  in
  let points_list = aux start_point [] in
  points_list
  |> List.map (Str.split (Str.regexp "_"))
  |> List.map (fun lst ->
    match lst with
    | x :: y :: _ -> int_of_string x, int_of_string y
    | _ -> failwith "Bad")
;;

let%test _ = highest_points_list test_matrix (0, 0) = [ 3, 0; 3, 2 ]

let map_scores_sum matrix =
  let rec aux (x, y) score =
    if x = Array.length matrix
    then score
    else if y = Array.length matrix.(0)
    then aux (x + 1, 0) score
    else if matrix.(x).(y) = 0
    then (
      let highest_points_list = highest_points_list matrix (x, y) in
      aux (x, y + 1) (score + List.length highest_points_list))
    else aux (x, y + 1) score
  in
  aux (0, 0) 0
;;

let map_ratings_sum matrix =
  let rec aux (x, y) score =
    if x = Array.length matrix
    then score
    else if y = Array.length matrix.(0)
    then aux (x + 1, 0) score
    else if matrix.(x).(y) = 0
    then (
      let highest_points_list = highest_points_list_all matrix (x, y) in
      aux (x, y + 1) (score + List.length highest_points_list))
    else aux (x, y + 1) score
  in
  aux (0, 0) 0
;;

[@@@ocamlformat "disable"]
let test_matrix_2 = [|
  [|8; 9; 0; 1; 0; 1; 2; 3|];
  [|7; 8; 1; 2; 1; 8; 7; 4|];
  [|8; 7; 4; 3; 0; 9; 6; 5|];
  [|9; 6; 5; 4; 9; 8; 7; 4|];
  [|4; 5; 6; 7; 8; 9; 0; 3|];
  [|3; 2; 0; 1; 9; 0; 1; 2|];
  [|0; 1; 3; 2; 9; 8; 0; 1|];
  [|1; 0; 4; 5; 6; 7; 3; 2|]
|]
[@@@ocamlformat "enable"]

let%test _ = map_scores_sum test_matrix_2 = 36
let%test _ = map_ratings_sum test_matrix_2 = 81

let solve_part1 str_list =
  str_list
  |> Utils.char_matrix_of_string_list
  |> Array.map (Array.map Utils.char_to_int)
  |> map_scores_sum
;;

let solve_part2 str_list =
  str_list
  |> Utils.char_matrix_of_string_list
  |> Array.map (Array.map Utils.char_to_int)
  |> map_ratings_sum
;;
