(*
  1. find_value_point matrix value
  2. another matrix to track visited
  3. if obstackle change direction and run again, we do not add redundant point because we store visited and after calc all points
  4. check if outside then finish [do first]
*)


let find_first_one_of (matrix: char array array) (values: char list) =
  let rec aux i j =
    if i = Array.length matrix then (' ', (-1, -1))
    else if j = Array.length matrix.(i) then aux (i + 1) 0
    else
      let cell = matrix.(i).(j) in
      let found_value = List.find_opt (fun (value) -> 
          cell = value
        ) values
      in
      match found_value with
      | Some(v) -> (v, (i, j))
      | None -> aux i (j + 1) 
  in

  aux 0 0

let test_matrix = [|
  [| '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '#'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '#'; '.'; '.'; '^'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.' |];
  [| '#'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.' |];
|]
 
let%test _ = find_first_one_of test_matrix ['^'] = ('^', (6, 4))

let get_next_point matrix x y dir_char = 
  let (next_x, next_y) = match dir_char with
    | 'v' -> (x+1), y
    | '<' -> x, (y-1)
    | '^' -> (x-1), y
    | '>' -> x, (y+1)
    | _ -> failwith "Unknwon direction"
  in

  if Utils.coord_outside matrix next_x next_y then None
  else Some(next_x, next_y)

let get_next_direction dir_char =
  match dir_char with
  | '^' -> '>'
  | '>' -> 'v'
  | 'v' -> '<'
  | '<' -> '^'
  | _ -> failwith "Unknwon direction"  

module StringSet = Set.Make(String) 

let get_obstackle_key x y dir =
  Printf.sprintf "%d_%d_%c" x y dir

let%test _ = get_obstackle_key 1 1 '<' = "1_1_<"
  
let get_visited_matrix (_matrix: char array array) =
  let matrix = Utils.copy_matrix _matrix in
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  let visited_matrix = Array.make_matrix rows columns false in
  let (_, (start_x, start_y)) = find_first_one_of matrix ['^'; 'v'; '<'; '>'] in
  let rec aux x y obstackles_set =
    let cell = matrix.(x).(y) in
    if cell != '#' then begin
      visited_matrix.(x).(y) <- true
    end;
    match get_next_point matrix x y cell with
    | Some(next_x, next_y) -> 
      if matrix.(next_x).(next_y) = '#' then begin
        let obstackle_key = get_obstackle_key next_x next_y cell in
        if StringSet.mem obstackle_key obstackles_set then true
        else begin
          matrix.(x).(y) <- (get_next_direction cell);
      
          aux x y (obstackles_set |> (StringSet.add obstackle_key))
        end;
      end else begin
        matrix.(x).(y) <- matrix.(next_x).(next_y);
        matrix.(next_x).(next_y) <- cell;

        aux next_x next_y obstackles_set
      end;
    | None -> false
  in

  let looped = aux start_x start_y StringSet.empty in
  visited_matrix, looped

let sum_bool_matrix matrix = 
  matrix
  |> Array.map (fun row -> Array.map (fun cell -> if cell = true then 1 else 0) row)
  |> Array.map (fun row -> Array.fold_left (+) 0 row )
  |> Array.fold_left (+) 0

let%test _ = 
  let visited_matrix, looped = get_visited_matrix test_matrix in
  let visited_count = sum_bool_matrix visited_matrix in

  visited_count = 41 && looped = false

let test_looped_matrix = [|
  [| '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '#'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '#'; '.'; '#'; '^'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.' |];
  [| '#'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.' |];
|]  
let%test _ = 
  let _, looped = get_visited_matrix test_looped_matrix in
  looped = true

let find_loop_count matrix =
  let (_, (start_x, start_y)) = find_first_one_of matrix ['^'; 'v'; '<'; '>'] in
  let rows, columns = Array.length matrix, Array.length matrix.(0) in

  let rec aux x y count =
    if x = rows then count
    else if y = columns then aux (x+1) 0 count
    else if x = start_x && y = start_y then aux x (y+1) count
    else begin
      let cell = matrix.(x).(y) in
      if cell = '#' then aux x (y+1) count
      else begin
        let matrix_copy = Utils.copy_matrix matrix in
        matrix_copy.(x).(y) <- '#';

        let _, looped = get_visited_matrix matrix_copy in
        if looped then aux x (y+1) (count+1)
        else aux x (y+1) count
      end
    end
  in

  aux 0 0 0

let%test _ = find_loop_count test_matrix = 6

let solve_part1 str =
  let matrix = Utils.char_matrix_of_string_list str in
  let visited_matrix, _ = get_visited_matrix matrix in
  sum_bool_matrix visited_matrix

let solve_part2 str =
  let matrix = Utils.char_matrix_of_string_list str in
  find_loop_count matrix