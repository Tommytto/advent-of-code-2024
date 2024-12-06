let sum a b = a + b
let%test _ = sum 10 10 = 20

let rec list_of_string_pattern pattern str start =
  try
    let start_idx = Str.search_forward pattern str start in
    let matched = Str.matched_string str in
    let next_start_idx = start_idx + String.length matched in
    matched :: list_of_string_pattern pattern str next_start_idx
  with Not_found -> []

let mul_regexp = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}
let%test _ = list_of_string_pattern mul_regexp 
  "mul(1,2)mul(11)mul(11,223)" 0 = 
  ["mul(1,2)"; "mul(11,223)"]

let rec find_operation_nums ?(start=0) pattern str =
  try
    let start_idx = Str.search_forward pattern str start in
    let matched = Str.matched_string str in
    let num_1 = Str.matched_group 1 str in
    let num_2 = Str.matched_group 2 str in
    let next_start_idx = start_idx + String.length matched in
    (num_1, num_2) :: find_operation_nums pattern str ~start:next_start_idx
  with Not_found -> []

let%test _ = find_operation_nums mul_regexp "mul(1,2)" = [("1", "2")]
let%test _ = find_operation_nums mul_regexp "mul(1,2)mul(11,12)" = [("1", "2"); ("11", "12")]

let rec find_operation_nums_with_idx ?(start=0) pattern str =
  try
    let start_idx = Str.search_forward pattern str start in
    let matched = Str.matched_string str in
    let num_1 = Str.matched_group 1 str in
    let num_2 = Str.matched_group 2 str in
    let next_start_idx = start_idx + String.length matched in
    (start_idx, num_1, num_2) :: find_operation_nums_with_idx pattern str ~start:next_start_idx
  with Not_found -> []

let%test _ = find_operation_nums_with_idx mul_regexp "mul(1,2)" = [(0, "1", "2")]
let%test _ = find_operation_nums_with_idx mul_regexp "mul(1,2)mul(11,12)" = [(0, "1", "2"); (8, "11", "12")]

let rec list_of_idx_pattern ?(start=0) pattern str =
  try
    let start_idx = Str.search_forward pattern str start in
    let matched = Str.matched_string str in
    let next_start_idx = start_idx + String.length matched in
    start_idx :: list_of_idx_pattern ~start:next_start_idx pattern str
  with Not_found -> []

let%test _ = list_of_idx_pattern (Str.regexp {|hi|}) "hi  hi" = [0; 4]
let%test _ = list_of_idx_pattern (Str.regexp {|don't()|}) "don't()" = [0]
let%test _ = list_of_idx_pattern (Str.regexp {|do()|}) "do()" = [0]

let multiply_string_pairs lst = 
  lst
  |> List.map (fun ((x,y)) -> (int_of_string x, int_of_string y))
  |> List.map (fun ((x,y)) -> x * y)
  |> Utils.sum_int_list

let find_multiply_nums str = 
  find_operation_nums mul_regexp str
  |> multiply_string_pairs


let find_operation_nums_with_breaks str =
  let pattern = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|don't()\|do()|} in
  let enabled = ref true in
  let rec aux start =
    try
      let start_idx = Str.search_forward pattern str start in
      let matched = Str.matched_string str in
      let next_start_idx = start_idx + String.length matched in
      match matched with
      | "do()" -> enabled := true; aux next_start_idx
      | "don't()" -> enabled := false; aux next_start_idx
      | _ ->
        let num_1 = Str.matched_group 1 str in
        let num_2 = Str.matched_group 2 str in
        if !enabled then (num_1, num_2) :: aux next_start_idx
        else aux next_start_idx
    with Not_found -> []
  in
  aux 0

let%test _ = find_operation_nums_with_breaks "mul(1,2)don't()mul(2,3)do()mul(5,6)" = [("1", "2"); ("5", "6")]

let solve_part1 lines_lst =
  Utils.string_list_join lines_lst "\n"
  |> find_multiply_nums



let solve_part2 linest_lst = 
  Utils.string_list_join linest_lst "\n"
  |> find_operation_nums_with_breaks
  |> multiply_string_pairs