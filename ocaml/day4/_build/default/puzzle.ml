let masks = 
  [
    [(0, 0); (0, 1); (0, 2); (0, 3)]; (* right *)
    [(0, 0); (1, 1); (2, 2); (3, 3)]; (* right-bottom *)
    [(0, 0); (1, 0); (2, 0); (3, 0)]; (* bottom *)
    [(0, 0); (1, -1); (2, -2); (3, -3)]; (* left-bottom *)
    [(0, 0); (0, -1); (0, -2); (0, -3)]; (* left *)
    [(0, 0); (-1, -1); (-2, -2); (-3, -3)]; (* left-top *)
    [(0, 0); (-1, 0); (-2, 0); (-3, 0)]; (* top *)
    [(0, 0); (-1, 1); (-2, 2); (-3, 3)] (* right-top *)
  ]

let coord_outside matrix x y =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  x >= rows || x < 0 || y >= columns || y < 0

let str_mask_count matrix (masks: ((int * int) list) list) (str_list: char list) = 
  
  let filter_fn x y mask = List.for_all2 (fun (x_step, y_step) search ->
      let x_new, y_new = x + x_step, y + y_step in
      let outside = coord_outside matrix x_new y_new in
      not outside && matrix.(x_new).(y_new) = search
  ) mask str_list in

  let (total_sum, _) = Array.fold_left (fun (total_sum, i) row -> 
    let (row_sum, _) = Array.fold_left (fun (row_sum, j) _ ->
      let filter_i_j = filter_fn i j in
      let found_cells = List.filter filter_i_j masks in
      let cell_sum = List.length found_cells in

      (row_sum + cell_sum, j+1)
    ) (0, 0) row in
    ((total_sum + row_sum), i+1)
  ) (0, 0) matrix in
  total_sum


let %test _ = 
  let matrix = [|
    [| 'M'; 'M'; 'M'; 'S'; 'X'; 'X'; 'M'; 'A'; 'S'; 'M' |];
    [| 'M'; 'S'; 'A'; 'M'; 'X'; 'M'; 'S'; 'M'; 'S'; 'A' |];
    [| 'A'; 'M'; 'X'; 'S'; 'X'; 'M'; 'A'; 'A'; 'M'; 'M' |];
    [| 'M'; 'S'; 'A'; 'M'; 'A'; 'S'; 'M'; 'S'; 'M'; 'X' |];
    [| 'X'; 'M'; 'A'; 'S'; 'A'; 'M'; 'X'; 'A'; 'M'; 'M' |];
    [| 'X'; 'X'; 'A'; 'M'; 'M'; 'X'; 'X'; 'A'; 'M'; 'A' |];
    [| 'S'; 'M'; 'S'; 'M'; 'S'; 'A'; 'S'; 'X'; 'S'; 'S' |];
    [| 'S'; 'A'; 'X'; 'A'; 'M'; 'A'; 'S'; 'A'; 'A'; 'A' |];
    [| 'M'; 'A'; 'M'; 'M'; 'M'; 'X'; 'M'; 'M'; 'M'; 'M' |];
    [| 'M'; 'X'; 'M'; 'X'; 'A'; 'X'; 'M'; 'A'; 'S'; 'X' |];
  |] in
  let search = ['X'; 'M'; 'A'; 'S'] in
  str_mask_count matrix masks search
  = 18

let char_matrix_of_string_list str_list =
  str_list
    |> List.map String.to_seq
    |> List.map List.of_seq
    |> List.map Array.of_list
    |> Array.of_list

let solve_part1 (str_list: string list): int = 
  let search = ['X'; 'M'; 'A'; 'S'] in
  let matrix = char_matrix_of_string_list str_list in
  
  str_mask_count matrix masks search


let x_mas_masks = 
  [
    ((-1, -1), (1, 1));
    ((-1, 1), (1, -1))
  ]

let both_exists c1 c2 ch1 ch2 =
  if c1 = ch1 && c2 = ch2 then true
  else if c1 = ch2 && c2 = ch1 then true
  else false

let str_mask_count_2 matrix = 
  let (total_sum, _) = Array.fold_left (fun (total_sum, i) row -> 
    let (row_sum, _) = Array.fold_left (fun (row_sum, j) cell ->
      let aux = 
        if cell != 'A' then false
        else if 
          coord_outside matrix (i-1) (j-1) || 
          coord_outside matrix (i+1) (j+1) ||
          coord_outside matrix (i-1) (j+1) ||
          coord_outside matrix (i+1) (j-1)
          then false
        else
          both_exists matrix.(i-1).(j-1) matrix.(i+1).(j+1) 'M' 'S'
          &&
          both_exists matrix.(i-1).(j+1) matrix.(i+1).(j-1) 'M' 'S'
      in

      match aux with
      | false -> (row_sum, j+1)
      | true -> (row_sum+1, j+1)
    ) (0, 0) row in
  
    ((total_sum + row_sum), i+1)
  ) (0, 0) matrix in
  total_sum

let %test _ = 
  let matrix = [|
    [| '.'; 'M'; '.'; 'S'; '.'; '.'; '.'; '.'; '.'; '.' |];
    [| '.'; '.'; 'A'; '.'; '.'; 'M'; 'S'; 'M'; 'S'; '.' |];
    [| '.'; 'M'; '.'; 'S'; '.'; 'M'; 'A'; 'A'; '.'; '.' |];
    [| '.'; '.'; 'A'; '.'; 'A'; 'S'; 'M'; 'S'; 'M'; '.' |];
    [| '.'; 'M'; '.'; 'S'; '.'; 'M'; '.'; '.'; '.'; '.' |];
    [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
    [| 'S'; '.'; 'S'; '.'; 'S'; '.'; 'S'; '.'; 'S'; '.' |];
    [| '.'; 'A'; '.'; 'A'; '.'; 'A'; '.'; 'A'; '.'; '.' |];
    [| 'M'; '.'; 'M'; '.'; 'M'; '.'; 'M'; '.'; 'M'; '.' |];
    [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |]
  |] in
  str_mask_count_2 matrix = 9

let solve_part2 (str_list: string list): int = 
  let matrix = char_matrix_of_string_list str_list in
  
  str_mask_count_2 matrix