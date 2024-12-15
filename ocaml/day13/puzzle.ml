(* decompose
   1. tree traversal, dfs or bfs actually doesn't matter in such case. UPD.
   by the way actually here dfs is better because all decisions are in the end of the tree,
   so its useless to store all variants
   2. with every branch pass previous position and tokens spent
   UPD. actually I think its helpful to store the whole path to calc later, probably for the part 2
   the question is how to store the path in OCAML???
   probably we can pass the current path to the nodes and
   what is the end of the recursion. we can mix and after every successfull result join flatten path

   4. then we need go through the tree

   UPD. Tree is too long, I think we need setup cheapest price overall and start do minues cheapest and plus more expensive
*)

open Core

let apply_button x y btn =
  let btn_x, btn_y = btn in
  x + btn_x, y + btn_y
;;

let find_paths_to_prize btn_a btn_b prize =
  let prize_x, prize_y = prize in
  let h = Hashtbl.create (module String) in
  let rec find_paths (x, y) current_path prize_paths a_counter b_counter =
    if x = prize_x && y = prize_y
    then current_path :: prize_paths
    else if a_counter > 100 || b_counter > 100
    then prize_paths
    else if Hashtbl.mem h current_path
    then Hashtbl.find_exn h current_path
    else (
      Hashtbl.set h ~key:current_path ~data:prize_paths;
      let new_x_a, new_y_a = apply_button x y btn_a in
      let new_x_b, new_y_b = apply_button x y btn_b in
      let p1 =
        find_paths
          (new_x_a, new_y_a)
          ("A" ^ current_path)
          prize_paths
          (a_counter + 1)
          b_counter
      in
      let p2 =
        find_paths
          (new_x_b, new_y_b)
          ("B" ^ current_path)
          prize_paths
          a_counter
          (b_counter + 1)
      in
      p1 @ p2)
  in
  find_paths (0, 0) "" [] 0 0
;;

(* let%test _ =
   List.equal
   String.equal__local
   (find_paths_to_prize (10, 5) (5, 10) (15, 15))
   [ "AB"; "BA" ]
   ;; *)

let calc_path_price (path : string) =
  path
  |> String.to_list
  |> List.fold ~init:0 ~f:(fun acc btn_id -> acc + if Char.equal btn_id 'A' then 3 else 1)
;;

let find_cheapest_path_price_of_paths (paths_list : string list) =
  paths_list |> List.map ~f:calc_path_price |> List.min_elt ~compare:Int.compare
;;

let make_key a b = string_of_int a ^ "_" ^ string_of_int b

let find_cheapest_price_using_tree btn_a btn_b prize =
  let prize_x, prize_y = prize in
  let h = Hashtbl.create (module String) in
  let rec find_paths (x, y) current_price min_price a_counter b_counter =
    if x = prize_x && y = prize_y
    then if current_price < min_price then current_price else min_price
    else if current_price > min_price
    then min_price
    else if a_counter > 100 || b_counter > 100
    then min_price
    else if Hashtbl.mem h (make_key a_counter b_counter)
    then Hashtbl.find_exn h (make_key a_counter b_counter)
    else (
      Hashtbl.set h ~key:(make_key a_counter b_counter) ~data:min_price;
      let new_x_a, new_y_a = apply_button x y btn_a in
      let new_x_b, new_y_b = apply_button x y btn_b in
      let p1 =
        find_paths
          (new_x_a, new_y_a)
          (current_price + 3)
          min_price
          (a_counter + 1)
          b_counter
      in
      let p2 =
        find_paths
          (new_x_b, new_y_b)
          (current_price + 1)
          min_price
          a_counter
          (b_counter + 1)
      in
      if p1 < p2 then p1 else p2)
  in
  find_paths (0, 0) 0 Int.max_value 0 0
;;

(* takes too long time *)
let find_cheapest_price_using_paths btn_a btn_b prize =
  let paths_list = find_paths_to_prize btn_a btn_b prize in
  match find_cheapest_path_price_of_paths paths_list with
  | Some price -> price
  | None -> -1
;;

let get_start_price (b_x, b_y) (prize_x, prize_y) =
  let min_price_by_x = prize_x / b_x in
  let min_price_by_y = prize_y / b_y in
  Int.min min_price_by_x min_price_by_y
;;

let find_cheapest_price_using_wrong_method
  (a_cost, a_x, a_y)
  (b_cost, b_x, b_y)
  (prize_x, prize_y)
  =
  let start_min_price = get_start_price (b_x, b_y) (prize_x, prize_y) in
  let start_x, start_y = start_min_price * b_x, start_min_price * b_y in
  Stdio.printf "(%d, %d) %d\n" start_x start_y start_min_price;
  let rec aux current_x current_y current_price =
    if current_x = prize_x && current_y = prize_y
    then Some current_price
    else if current_x < 0 || current_y < 0 || (current_x > prize_x && current_y > prize_y)
    then (
      Stdio.printf "(%d, %d) %d\n" current_x current_y current_price;
      None)
    else (
      Stdio.printf "(%d, %d) %d\n" current_x current_y current_price;
      aux (current_x - b_x + a_x) (current_y - b_y + a_y) (current_price - b_cost + a_cost))
  in
  aux (start_x + a_x) (start_y + a_y) (start_min_price + 3)
;;

let%test _ = Int.equal (find_cheapest_price_using_tree (94, 34) (22, 67) (8400, 5400)) 280

let parse_input_item str =
  let regexp_a = Re.Pcre.regexp ".*?(\\d+).*Y.*?(\\d+)" in
  match Re.exec_opt regexp_a str with
  | Some groups ->
    let num1 = Re.Group.get groups 1 in
    let num2 = Re.Group.get groups 2 in
    int_of_string num1, int_of_string num2
  | None -> failwith "bad input"
;;

let%test _ = Stdlib.( = ) (parse_input_item "Button A: X+12, Y+48") (12, 48)

type string_list = string list [@@deriving show]

let parse_input str =
  str
  |> Str.split (Str.regexp "\n\n")
  |> List.filter ~f:(fun l -> not (String.equal l ""))
  |> List.map ~f:(Str.split (Str.regexp "\n"))
  |> List.map ~f:(fun task_list ->
    match task_list with
    | a :: b :: prize :: _ ->
      parse_input_item a, parse_input_item b, parse_input_item prize
    | _ -> failwith "oh my")
;;

let find_cheapest_price_using_tree_v2 a b (prize_x, prize_y) =
  let part1 = find_cheapest_price_using_tree a b (prize_x + 10000, prize_y + 10000) in
  if phys_equal part1 Int.max_value
  then Int.max_value
  else (
    let part2 = find_cheapest_price_using_tree a b (10000, 10000) in
    if phys_equal part2 Int.max_value then Int.max_value else part1 + 1000000000)
;;

let%test _ = Int.equal (find_cheapest_price_using_tree (94, 34) (22, 67) (8400, 5400)) 280

let solve_equation (a_x, a_y) (b_x, b_y) (p_x, p_y) =
  let snd_button_times = ((a_x * p_y) - (a_y * p_x)) / ((-a_y * b_x) + (a_x * b_y)) in
  let fst_button_times = (p_x - (b_x * snd_button_times)) / a_x in
  fst_button_times, snd_button_times
;;

let get_buttons_cost (a_x, a_y) (b_x, b_y) (p_x, p_y) a_times b_times =
  if (not (phys_equal ((a_x * a_times) + (b_x * b_times)) p_x))
     || not (phys_equal ((a_y * a_times) + (b_y * b_times)) p_y)
  then None
  else Some ((a_times * 3) + (b_times * 1))
;;

let solve_part1 str =
  parse_input str
  |> List.map ~f:(fun (a, b, prize) -> find_cheapest_price_using_tree a b prize)
  |> List.filter ~f:(fun result -> not (phys_equal result Int.max_value))
  |> List.fold ~init:0 ~f:( + )
;;

let err_delta = 10_000_000_000_000

let solve_part2 str =
  parse_input str
  |> List.map ~f:(fun (a, b, (p_x, p_y)) -> a, b, (p_x + err_delta, p_y + err_delta))
  |> List.map ~f:(fun (a, b, prize) ->
    let a_times, b_times = solve_equation a b prize in
    get_buttons_cost a b prize a_times b_times)
  |> List.filter ~f:Option.is_some
  |> List.map ~f:(fun t -> Option.value_exn t)
  |> List.fold ~init:0 ~f:(fun acc t -> acc + t)
;;
