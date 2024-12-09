(* decompose
  we need to find antennas with same frequence, 
  + - get_antinode_positions matrix pair(x,y) -> list[(x,y)] -> antinodes_matrix
  + - get_antennas_pairs_variations antennas_list -> list[pair(x,y)]
  + - make_antennas_map_list
  - calc_unique_antinodes antinotes_matrix
*)

let points_vector (x1, y1) (x2, y2) =
  x1 - x2, y1 - y2

let%test _ = points_vector (3,4) (5,5) = (-2, -1)
(* (1,3) (7,6) *)

(* (5,5) (4,8) *)
let antinode_positions (x1, y1) (x2, y2) =
  let (vec_x, vec_y) = points_vector (x1, y1) (x2, y2) in

  [
    (x1 + vec_x, y1 + vec_y);
    (x2 - vec_x, y2 - vec_y);
  ]

let%test _ = antinode_positions (3,4) (5,5) = [
  (1, 3);
  (7, 6);
]

let antinode_positions_v2 matrix (x1, y1) (x2, y2) =
  let (vec_x, vec_y) = points_vector (x1, y1) (x2, y2) in

  let rec aux x y vec acc = 
    let vec_x, vec_y = vec in
    let new_x, new_y = x + vec_x, y + vec_y in
    if Utils.coord_outside matrix new_x new_y then acc
    else aux new_x new_y vec ((new_x, new_y) :: acc)
  in

  aux x1 y1 (vec_x, vec_y) [] @ aux x2 y2 (-vec_x, -vec_y) [] @ [(x1, y1); (x2, y2)]

let test_matrix = [|
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; 'a'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; 'a'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
|]

let%test _ = antinode_positions_v2 test_matrix (3,4) (5,5) = [
  (1, 3);
  (9, 7);
  (7, 6);
  (3, 4);
  (5, 5);
]

module CharMap = Map.Make(Char)

let make_antennas_map_list matrix = 
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in

  let rec aux x y result_map =
    if x >= rows then result_map
    else if y >= columns then aux (x+1) 0 result_map
    else
      let cell = matrix.(x).(y) in
      if cell != '.' && cell != '#' then
        let new_list = match CharMap.find_opt cell result_map with
          | Some(lst) -> lst @ [(x,y)]
          | None -> [(x,y)]
        in

        aux x (y+1) (result_map |> CharMap.add cell new_list)
      else
        aux x (y+1) result_map
  in

  aux 0 0 CharMap.empty
  

let%test _ = make_antennas_map_list test_matrix = 
  CharMap.(
    empty
    |> add 'a' [(3,4); (5,5)]
  )

let get_antennas_pairs_variations pairs_list = 
  let rec aux lst result = 
    match lst with
    | [_] -> result
    | hd :: tl -> 
      let pairs = List.map (fun point -> (hd, point)) tl in
      aux tl (result @ pairs)
    | _ -> failwith "Alarm"
  in

  aux pairs_list []

let%test _ = get_antennas_pairs_variations [(3,4); (5,5); (1,2)] = [((3, 4), (5, 5)); ((3, 4), (1, 2)); ((5, 5), (1, 2))]


let get_antinodes antinode_getter matrix =
  let antennas_map = make_antennas_map_list matrix in
  CharMap.fold (fun _ points acc -> 
    let pairs_variations = get_antennas_pairs_variations points in
    let antinode_positions_list = List.map (fun (node1, node2) ->
      antinode_getter node1 node2
    ) pairs_variations
    in

    let b = List.flatten antinode_positions_list in

    acc @ b
  ) antennas_map []

let get_antinodes_v1 matrix = get_antinodes antinode_positions matrix
let get_antinodes_v2 matrix = get_antinodes (antinode_positions_v2 matrix) matrix

let calc_unique_antinodes get_antinodes_fn matrix = 
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  let all_antinodes = get_antinodes_fn matrix in
  let antinodes_matrix = Array.make_matrix rows columns 0 in

  all_antinodes
  |> List.filter (fun (x, y) -> not (Utils.coord_outside matrix x y))
  |> List.iter (fun (x, y) ->
    antinodes_matrix.(x).(y) <- 1;
  );

  antinodes_matrix
  |> Array.map (Array.fold_left (+) 0)
  |> Array.fold_left (+) 0

let test_matrix_2 = [|
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '0'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; 'A'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; 'A'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; 'A'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
  [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |];
|]
let%test _ = calc_unique_antinodes get_antinodes_v1 test_matrix_2 = 14
let%test _ = calc_unique_antinodes get_antinodes_v2 test_matrix_2 = 34

let solve_part1 str_list =
  str_list
  |> Utils.char_matrix_of_string_list 
  |> calc_unique_antinodes get_antinodes_v1

let solve_part2 str_list =
  str_list
  |> Utils.char_matrix_of_string_list 
  |> calc_unique_antinodes get_antinodes_v2