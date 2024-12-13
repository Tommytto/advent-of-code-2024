(* decompose
   1. iterate over matrix
   2. find first letter and search only them
   3. store visited in separate matrix
   4. rec call of neighbours
*)

open Core

let dirs_masks = [ 0, 1; 1, 0; 0, -1; -1, 0 ]

let fence_size (matrix : char array array) (x, y) =
  let cell = matrix.(x).(y) in
  List.fold dirs_masks ~init:0 ~f:(fun acc (x_step, y_step) ->
    let new_x, new_y = x + x_step, y + y_step in
    acc
    +
    if Utils.coord_outside matrix (new_x, new_y)
       || Char.( <> ) matrix.(new_x).(new_y) cell
    then 1
    else 0)
;;

[@@@ocamlformat "disable"]
let test_small = [|
  [| 'A'; 'A'; 'A'; 'A' |];
  [| 'B'; 'B'; 'C'; 'D' |];
  [| 'B'; 'B'; 'C'; 'C' |];
  [| 'E'; 'E'; 'E'; 'C' |];
|]
[@@@ocamlformat "enable"]

let%test _ = fence_size test_small (0, 0) = 3
let%test _ = fence_size test_small (0, 1) = 2

let find_regions matrix x y visited =
  let rec aux x y =
    if visited.(x).(y)
    then 'f', 0, 0
    else (
      let letter = matrix.(x).(y) in
      visited.(x).(y) <- true;
      let fence = fence_size matrix (x, y) in
      let rest_region_area, rest_region_fence_size =
        List.fold
          dirs_masks
          ~init:(0, 0)
          ~f:(fun (acc_area, acc_perimeter) (step_x, step_y) ->
            let new_x, new_y = x + step_x, y + step_y in
            if Utils.coord_outside matrix (new_x, new_y)
               || Char.( <> ) matrix.(new_x).(new_y) letter
            then acc_area, acc_perimeter
            else (
              let _, n_area, n_perimeter = aux new_x new_y in
              acc_area + n_area, acc_perimeter + n_perimeter))
      in
      letter, rest_region_area + 1, rest_region_fence_size + fence)
  in
  aux x y
;;

let sum a b = a + b
let sum100 = sum 100
let%test _ = sum 4 5 = 9
let%test _ = sum100 4 = 104

let calc_corners_count matrix x y =
  let start_letter = matrix.(x).(y) in
  let rec aux dirs count =
    match dirs with
    | [ _ ] -> count
    | [] -> failwith "shit happened"
    | p1 :: p2 :: tl ->
      let (dx1, dy1), (dx2, dy2) = p1, p2 in
      let nx1, nx2, ny1, ny2 = x + dx1, x + dx2, y + dy1, y + dy2 in
      let corner =
        if (Utils.coord_outside matrix (nx1, ny1)
            || Char.( <> ) matrix.(nx1).(ny1) start_letter)
           && (Utils.coord_outside matrix (nx2, ny2)
               || Char.( <> ) matrix.(nx2).(ny2) start_letter)
        then 1
        else if (not (Utils.coord_outside matrix (nx1, ny1)))
                && (not (Utils.coord_outside matrix (nx2, ny2)))
                && Char.equal matrix.(nx1).(ny1) start_letter
                && Char.equal matrix.(nx2).(ny2) start_letter
                && (Utils.coord_outside matrix (x + dx1 + dx2, y + dy1 + dy2)
                    || Char.( <> ) matrix.(x + dx1 + dx2).(y + dy1 + dy2) start_letter)
        then 1
        else 0
      in
      aux (p2 :: tl) (count + corner)
  in
  aux (dirs_masks @ [ List.hd_exn dirs_masks ]) 0
;;

[@@@ocamlformat "disable"]
let test_small_2 = [|
  [| 'A'; 'A'; 'A'; 'A' |];
  [| 'B'; 'B'; 'C'; 'D' |];
  [| 'B'; 'B'; 'C'; 'C' |];
  [| 'E'; 'E'; 'E'; 'C' |];
|]

let c =[(1,2); (2, 2); (2, 3); (3, 3)]
[@@@ocamlformat "enable"]

let%test _ = calc_corners_count test_small_2 0 0 = 2
let%test _ = calc_corners_count test_small_2 1 2 = 2
let%test _ = calc_corners_count test_small_2 2 2 = 2

let find_region_params matrix x y visited =
  let rec aux x y =
    if visited.(x).(y)
    then 'f', 0, 0, 0
    else (
      let letter = matrix.(x).(y) in
      visited.(x).(y) <- true;
      let fence = fence_size matrix (x, y) in
      let corners_count = calc_corners_count matrix x y in
      let rest_region_area, rest_region_fence_size, rest_corners_count =
        List.fold
          dirs_masks
          ~init:(0, 0, 0)
          ~f:(fun (acc_area, acc_perimeter, acc_corners_count) (step_x, step_y) ->
            let new_x, new_y = x + step_x, y + step_y in
            if Utils.coord_outside matrix (new_x, new_y)
               || Char.( <> ) matrix.(new_x).(new_y) letter
            then acc_area, acc_perimeter, acc_corners_count
            else (
              let _, n_area, n_perimeter, n_corners_count = aux new_x new_y in
              ( acc_area + n_area
              , acc_perimeter + n_perimeter
              , acc_corners_count + n_corners_count )))
      in
      ( letter
      , rest_region_area + 1
      , rest_region_fence_size + fence
      , rest_corners_count + corners_count ))
  in
  aux x y
;;

let calc_fencing_price matrix =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  let visited = Array.make_matrix ~dimx:rows ~dimy:columns false in
  let rec aux x y all_regions =
    if x = rows
    then all_regions
    else if y = columns
    then aux (x + 1) 0 all_regions
    else if visited.(x).(y)
    then aux x (y + 1) all_regions
    else (
      let params = find_region_params matrix x y visited in
      aux x (y + 1) (params :: all_regions))
  in
  let result = aux 0 0 [] in
  let price1 =
    List.fold result ~init:0 ~f:(fun acc (_, area, perimiter, _) ->
      acc + (area * perimiter))
  in
  let price2 =
    List.fold result ~init:0 ~f:(fun acc (_, area, _, sides) -> acc + (area * sides))
  in
  price1, price2
;;

[@@@ocamlformat "disable"]
let test_matrix = Utils.char_matrix_of_string "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

let matrix_o = [|
  [| 'O'; 'O'; 'O'; 'O'; 'O' |];
  [| 'O'; 'X'; 'O'; 'X'; 'O' |];
  [| 'O'; 'O'; 'O'; 'O'; 'O' |];
  [| 'O'; 'X'; 'O'; 'X'; 'O' |];
  [| 'O'; 'O'; 'O'; 'O'; 'O' |];
|]
[@@@ocamlformat "enable"]

let%test _ = fst (calc_fencing_price test_small) = 140
let%test _ = fst (calc_fencing_price matrix_o) = 772
let%test _ = fst (calc_fencing_price test_matrix) = 1930
let%test _ = snd (calc_fencing_price test_small) = 80
let solve_part1 str = str |> Utils.char_matrix_of_string |> calc_fencing_price |> fst
let solve_part2 str = str |> Utils.char_matrix_of_string |> calc_fencing_price |> snd
