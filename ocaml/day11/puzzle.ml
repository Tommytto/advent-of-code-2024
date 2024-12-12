(* decompose
   1. blink lst -> new_lst
   2. iterate and blink
   3. count stones
*)

let parse_input str = String.split_on_char ' ' str |> List.map int_of_string
let%test _ = parse_input "0 1 2" = [ 0; 1; 2 ]

type t = int list [@@deriving show]

let blink lst =
  let rec aux lst result =
    match lst with
    | [] -> result
    | hd :: tl ->
      if hd = 0
      then aux tl (1 :: result)
      else (
        let hd_str = string_of_int hd in
        let hd_str_len = String.length hd_str in
        if hd_str_len mod 2 = 0
        then (
          let left_num = String.sub hd_str 0 (hd_str_len / 2) in
          let right_num = String.sub hd_str (hd_str_len / 2) (hd_str_len / 2) in
          aux tl (int_of_string right_num :: int_of_string left_num :: result))
        else aux tl ((hd * 2024) :: result))
  in
  List.rev (aux lst [])
;;

let%test _ = blink [ 0; 1; 10; 99; 999 ] = [ 1; 2024; 1; 0; 9; 9; 2021976 ]

let blink_n input n =
  let rec aux n result = if n = 0 then result else aux (n - 1) (blink result) in
  aux n input
;;

type pair = int * int

module TumpleMap = Map.Make (struct
    type t = int * int

    let compare = compare
  end)

(* let blink_with_map lst =
   let new_lst = blink lst in
   List.fold_left (fun num ->
   let key =
   ) *)

(* let make_blink_map_n start_num n =
   let rec aux n_left num dict =
   if n_left = 0
   then dict
   else (
   let new_lst = blink [ num ] in
   let key = num, n - n_left + 1 in
   let new_dict = TumpleMap.add key (List.length new_lst) dict in
   List.fold_left (fun acc number ->
   aux (n_left - 1) number acc
   ) new_dict new_lst)
   in
   aux n start_num TumpleMap.empty
   ;; *)

let count_stones_after_n_blinks stones blinks =
  let table = Hashtbl.create 1000 in
  let rec stones_after_blinks num blinks =
    if blinks = 0
    then 1
    else (
      match Hashtbl.find_opt table (num, blinks) with
      | Some res -> res
      | None ->
        let next_stones = blink [ num ] in
        let c =
          List.fold_left
            (fun acc num -> acc + stones_after_blinks num (blinks - 1))
            0
            next_stones
        in
        Hashtbl.add table (num, blinks) c;
        c)
  in
  List.fold_left (fun acc stone -> acc + stones_after_blinks stone blinks) 0 stones
;;

let solve_part1 str =
  let input = parse_input str in
  count_stones_after_n_blinks input 25
;;

let%test _ = solve_part1 "125 17" = 55312

let solve_part2 str =
  let input = parse_input str in
  count_stones_after_n_blinks input 75
;;

(* how to solve part2
   the main idea for now is on every iteration numbers are changing but repeating often,
   especially in terms of 0 1

   1. calc 0 n times, on every iteration store information (num, iteration, count)
   2. calc 1 n times, on every iteration check if in dict there is information with key num_iteration

   1. blink
   2. 0 with 25 is 1 with 24 is 2048 with 23 is (20) at 22 + (48) at 22 is
*)

(* let rec check3 start_num deep dict =
   let result = blink [ start_num ] in
   let key = (start_num, 1) in
   let dict_with_cur = TumpleMap.add key (List.length result) dict in
   if deep > 3
   then List.length result, dict_with_cur
   else
   List.fold_left
   (fun (counter, dict) number ->
   let new_counter, new_dict = check3 number (deep + 1) dict in
   counter + new_counter, new_dict)
   (0, dict_with_cur)
   result
   ;; *)
(* fill dict *)

(* let check2 start_lst n =
   let rec aux deep lst prev_to_update dict =
   let (blip_length, blip_dict, new_lst_rev) = List.fold_left (fun (total_count, dict, full_blink) number ->
   let blink_result = blink [number] in
   let blink_result_length = List.length blink_result in
   let key = (number, 1) in

   (total_count + blink_result_length, TumpleMap.add key blink_result_length dict, blink_result::full_blink)
   ) (0, dict, []) lst in
   let new_lst = List.rev new_lst_rev in

   (* let dict_with_cur  *)
   in *)

(* let check start_num n =
   let rec aux deep num update_lst dict total_counter =
   let new_lst = blink [ num ] in
   let key = num, deep in
   let dict_with_cur_key = TumpleMap.add key (List.length new_lst) dict in
   let new_update_list = (num, deep) :: update_lst in

   let (next_level_count, updated_dict) = List.fold_left (fun (count, dict) number ->
   let (next_level_counter, updated_dict) = aux (deep + 1) number new_update_list dict 0 in

   (count + next_level_counter, updated_dict)
   ) (0, dict_with_cur_key) new_lst in

   let new_dict =
   List.fold_left
   (fun acc (prev_num, prev_deep) ->
   let prev_key = prev_num, deep - prev_deep in
   TumpleMap.add prev_key (List.length new_lst) acc)
   dict_with_cur_key
   update_lst
   in

   in
   aux 1 start_num [] TumpleMap.empty
   ;; *)
