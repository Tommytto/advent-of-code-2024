(* 
  + 1. parse_input to ((target), [numbers])
  + 2. make_variations which returns list of possible operators variations - in: 2 out: [[+,*]; [*,+]]
  + 3. apply_operations in: nums, variation -> sum
  + 4. valid_operations in: nums, varation, target -> bool
  + 5. target_is_possible in: nums, variations, target -> bool
  6. filter and sum targets
 *)

 let parse_input str_list = 
  str_list
  |> List.map (fun str ->
      let parts = Str.split (Str.regexp ":") str in
      let left, right = match parts with
          | a :: b :: _ -> a, b
          | _ -> "", ""
      in

      let left_parsed = int_of_string left in
      let right_parsed = 
        (Str.split (Str.regexp " ") right)
        |> List.map int_of_string
      in

      left_parsed, right_parsed
    ) 

let test_input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

let%test _ = parse_input ["190: 10 19"; "3267: 81 40 27"] = [
  (190, [10;19]);
  (3267, [81;40;27]);
]

(* how to make vairations???? *)
(*
      0 0 0 0
*)
let make_variations (values: 'a list) count = 
  let rec aux (prefix: 'a list) (variations: 'a list list): 'a list list =
    if List.length prefix = count then [prefix]
    else
      let branch_variations = List.map (fun value ->
          aux (value :: prefix) variations
        ) values
      in


      
      List.flatten branch_variations
  in

  aux [] []

let%test _ = make_variations [0; 1] 2 = [
  [0;0];
  [1;0];
  [0;1];
  [1;1]
]

let mul_plus_var_maker = make_variations ["+"; "*";]
let mul_plus_or_var_maker = make_variations ["+"; "*"; "||"]

let join_nums a b =
  int_of_string (string_of_int a ^ string_of_int b)

let%test _ = join_nums 1 5 = 15
let%test _= join_nums 0 99 = 99

let apply_operations nums operators = 
  if List.length nums != ((List.length operators) + 1) then failwith "Bad nums or variation"
  else
    let rec aux nums operators: int =
      match nums with
      | fst :: snd :: tl -> 
        (match operators with
        | hd :: tl2 -> 
          (match hd with
          | "+" -> aux ((fst + snd) :: tl) tl2
          | "*" -> aux ((fst * snd) :: tl) tl2
          | "||" -> aux ((join_nums fst snd) :: tl) tl2
          | _ -> failwith "Unknown operation")
        | _ -> failwith "Impossible")
      | [result] -> result
      | _ -> failwith "Too low elements"
    in

    aux nums operators

let%test _ = apply_operations [3;4;5] ["+"; "*"] = 35
let%test _ = apply_operations [3;4;5;10] ["+"; "*"; "*"] = 350
let%test _ = apply_operations [3;4;5;10] ["+"; "||"; "*"] = 750

let are_correct_operations nums target operations =
  apply_operations nums operations = target

let target_is_possible variations_maker nums target = 
  let operations_variants_count = List.length nums - 1 in
  let operations_variants = variations_maker operations_variants_count in

  match List.find_opt (are_correct_operations nums target) operations_variants with
  | Some(_) -> true
  | _ -> false

let%test _= target_is_possible mul_plus_var_maker [3;4;5] 35 = true
let%test _= target_is_possible mul_plus_var_maker [3;4;5] 36 = false
let%test _= target_is_possible mul_plus_or_var_maker [3;4;5;10] 750 = true

let find_correct_expressions variations_maker expressions =
  expressions
  |> List.filter (fun (target, nums) -> target_is_possible variations_maker nums target)

let%test _ = find_correct_expressions mul_plus_var_maker [
  (35, [3;4;5]);
  (36, [3;4;5])
] = [
  (35, [3;4;5])
]

let solve_part1 str_list =
  str_list
  |> parse_input
  |> find_correct_expressions mul_plus_var_maker
  |> List.map (fun (target, _) -> target)
  |> List.fold_left (+) 0

let solve_part2 str_list =
  str_list
  |> parse_input
  |> find_correct_expressions mul_plus_or_var_maker
  |> List.map (fun (target, _) -> target)
  |> List.fold_left (+) 0