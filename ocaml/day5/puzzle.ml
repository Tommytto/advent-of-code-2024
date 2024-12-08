
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let get_should_before_map (num_pairs: (int * int) list) =
  List.fold_left (fun (acc: IntSet.t IntMap.t) pair -> 
    let num, num_to_be_before = pair in
    let num_set = 
      match IntMap.find_opt num acc with
      | Some(x) -> x
      | None -> IntSet.empty
    in
    let new_num_set = num_set |> IntSet.add num_to_be_before in
    
    acc |> IntMap.add num new_num_set
  ) IntMap.empty num_pairs

let%test _ = get_should_before_map [(1, 2); (2, 3); (2, 4)] = IntMap.(
  empty
  |> IntMap.add 1 (IntSet.(empty |> IntSet.add 2))
  |> IntMap.add 2 (IntSet.(empty |> IntSet.add 3 |> IntSet.add 4))
)

let parse_task_input str = 
  let part1, part2 = 
    match Str.split (Str.regexp "\n\n") str with
    | p1 :: p2 :: _ -> p1, p2
    | _ -> "", "" 
  in

  let rules_list =
    Str.split (Str.regexp "\n") part1
    |> List.map (fun rule_str -> Str.split (Str.regexp {|||}) rule_str)
    |> List.map (fun rule -> List.map int_of_string rule)
    |> List.map (fun rule_parts ->
        match rule_parts with
        | a :: b :: _ -> (a, b)
        | _ -> (-1, -1)
      )
  in

  let updates_list =
    Str.split (Str.regexp "\n") part2
    |> List.map (fun row -> Str.split (Str.regexp ",") row)
    |> List.map (fun row -> List.map int_of_string row)
  in

  rules_list, updates_list
  
let%test _ = parse_task_input "1|2\n\n1,2\n2,3" = (
  [(1, 2)],
  [
    [1; 2];
    [2; 3]
  ]
)

let valid_update (check_map: IntSet.t IntMap.t) (update: int list) = 
  let rec aux (lst: int list) = 
    match lst with
    | [] -> true
    | hd :: tl -> 
      let last_correct = List.for_all (fun j ->
        match IntMap.find_opt j check_map with
          | None -> true
          | Some(set) ->
            not (IntSet.mem hd set)
        ) tl
      in

      last_correct && aux tl
  in

  aux update

let test_set_map = IntMap.(
  empty
  |> IntMap.add 1 (IntSet.(empty |> IntSet.add 2))
  |> IntMap.add 2 (IntSet.(empty |> IntSet.add 3 |> IntSet.add 4))
)

let%test _ = valid_update test_set_map [1;2;3] = true 
let%test _ = valid_update test_set_map [3;2;1] = false

let int_list_middle lst = 
  let arr = Array.of_list lst in
  let n = Array.length arr in
  let middle_index = n / 2 in
  
  arr.(middle_index)

let%test _ = int_list_middle [1;2;3] = 2
let%test _ = int_list_middle [1;2;3;4] = 3

(*
  1. find invalid
  2. make list of correct sequence by iterating over map and find those for whom there is no fututre restrictions, 
  after adding in it remove from map
  3. after iterate through an array; if i number is in sorted set, then skip if not just append to set

    It turned out that just creating sorted array as head of final array is not final decision
    Because reports does not necessarily have all numbers from rules
    so we have a situation
    [1,2,3,4,5] sorted rule numbers and
    [1,5,4] update

    [1,5,2,4,3] -> [1,4,2,5,3] -> [1,3,2,5,4] -> [1,2,3,5,4] -> [1,2,3,4,5]
  *)

let rec find_first set_map (start: int) =
  IntMap.fold (fun k v acc -> 
      let start_number_should_go_after = IntSet.mem start v in
      if start_number_should_go_after then find_first set_map k
      else acc
  ) set_map start

let%test _ = 
  find_first (IntMap.(
    empty
    |> IntMap.add 4 (IntSet.(empty |> IntSet.add 1))
    |> IntMap.add 2 (IntSet.(empty |> IntSet.add 3 |> IntSet.add 4))
  )) 4 = 2



let solve_part1 str =
  let rules, updates = parse_task_input str in
  let should_before_map = get_should_before_map rules in
  
  updates
  |> List.filter (valid_update should_before_map)
  |> List.map int_list_middle
  |> List.fold_left (+) 0


let get_fixed_update check_map lst =
  let n = List.length lst in
  let rec aux arr (i: int) (j: int) =
    if i = n-1 then arr
    else if j = n then aux arr (i+1) (i+1+1)
    else
      let num1 = arr.(i) in
      let num2 = arr.(j) in

      let num1_valid_position = 
        match IntMap.find_opt num2 check_map with
        | None -> true
        | Some(set) ->
          not (IntSet.mem num1 set)
      in

      if num1_valid_position then aux arr i (j+1)
      else 
        let _ = Utils.array_swap arr i j in
        aux arr i j
  in

  let result = aux (Array.of_list lst) 0 1 in
  Array.to_list result
  
let%test _ = get_fixed_update (
  IntMap.(
  empty
  |> IntMap.add 1 (IntSet.(empty |> IntSet.add 2))
  |> IntMap.add 2 (IntSet.(empty |> IntSet.add 3 |> IntSet.add 4))
  |> IntMap.add 3 (IntSet.(empty |> IntSet.add 4))
) 
) [4;2;1;3] = [1;2;3;4]  

let solve_part2 str =
  let rules, updates = parse_task_input str in
  let should_before_map = get_should_before_map rules in
  
  updates
  |> List.filter (fun x -> not (valid_update should_before_map x))
  |> List.map (get_fixed_update should_before_map)
  |> List.map int_list_middle
  |> List.fold_left (+) 0