open Core

let numpad = Utils.char_matrix_of_string "789\n456\n123\n 0A"
let dirpad = Utils.char_matrix_of_string " ^A\n<v>"

let char_times ch times =
  let rec aux n = if n = 0 then [] else ch :: aux (n - 1) in
  aux times
;;

let find_index lst value =
  let index, _ = List.findi_exn lst ~f:(fun _ v -> equal_char v value) in
  index
;;

type cm = char array array [@@deriving show]
type cl = char list [@@deriving show]
type cll = char list list [@@deriving show]
type ctl = (char * char) list [@@deriving show]

let walk keypad y1 x1 (path : char list) =
  (* printf "walk: %s\n%d,%d\n%s\n\n\n" (show_cm keypad) y1 x1 (show_cl path); *)
  let rec aux y1 x1 (path : char list) =
    match path with
    | direction :: tl ->
      let neighbours = [ y1, x1 - 1; y1, x1 + 1; y1 - 1, x1; y1 + 1, x1 ] in
      let dir_index = find_index [ '<'; '>'; '^'; 'v' ] direction in
      let y, x =
        List.find_mapi_exn neighbours ~f:(fun idx n ->
          if idx = dir_index then Some n else None)
      in
      keypad.(y).(x) :: aux y x tl
    | _ -> []
  in
  aux y1 x1 path
;;

let paths_between keypad start_point end_point =
  (* printf "paths_between: %c %c\n" start_point end_point; *)
  let y1, x1 = Utils.find_char_pos keypad start_point in
  let y2, x2 = Utils.find_char_pos keypad end_point in
  let hor = if x2 > x1 then '>' else '<' in
  let hor_times = Int.abs (x2 - x1) in
  let ver = if y2 > y1 then 'v' else '^' in
  let ver_times = Int.abs (y2 - y1) in
  let hor_chars = char_times hor hor_times in
  let ver_chars = char_times ver ver_times in
  let paths = [ hor_chars @ ver_chars; ver_chars @ hor_chars ] in
  (* printf "paths: %s\n" (show_cll paths); *)
  List.filter paths ~f:(fun path ->
    let path_chars = walk keypad y1 x1 path in
    (* printf "path chars: %s\n" (show_cl path_chars); *)
    let good_path =
      List.filter path_chars ~f:(fun ch -> equal_char ch ' ') |> List.length = 0
    in
    good_path)
  |> List.map ~f:(fun p -> p @ [ 'A' ])
;;

let%test_unit _ =
  [%test_result: char list list]
    (paths_between dirpad '^' '<')
    ~expect:[ [ 'v'; '<'; 'A' ] ]
;;

let%test_unit _ =
  [%test_result: char list list]
    (paths_between numpad 'A' '0')
    ~expect:[ [ '<'; 'A' ]; [ '<'; 'A' ] ]
;;

let pairwise lst =
  let rec aux lst =
    match lst with
    | hd :: snd :: tl -> (hd, snd) :: aux (snd :: tl)
    | _ -> []
  in
  aux lst
;;

type cb = bool * char * char * int [@@deriving sexp, equal, hash, compare]

module Cb = struct
  type t = cb

  let sexp_of_t = sexp_of_cb
  let t_of_sexp = cb_of_sexp
  let hash = hash_cb
  let compare = compare_cb
end

let cost_between_cache = Hashtbl.create (module Cb)

type cc = bool * char list * int [@@deriving sexp, equal, hash, compare]

module Cc = struct
  type t = cc

  let sexp_of_t = sexp_of_cc
  let t_of_sexp = cc_of_sexp
  let hash = hash_cc
  let compare = compare_cc
end

let cost_cache = Hashtbl.create (module Cc)

let rec cost keypad keys links =
  let is_numeric_keypad = equal_char keypad.(0).(0) '7' in
  let cost_between keypad start endd links =
    let is_numeric_keypad = equal_char keypad.(0).(0) '7' in
    if Hashtbl.mem cost_between_cache (is_numeric_keypad, start, endd, links)
    then Hashtbl.find_exn cost_between_cache (is_numeric_keypad, start, endd, links)
    else if links = 0
    then 1
    else (
      let paths = paths_between keypad start endd in
      (* printf "paths: %s %c %c\n" (show_cll paths) start endd; *)
      let paths_costs = List.map paths ~f:(fun path -> cost dirpad path (links - 1)) in
      let min_cost =
        List.min_elt paths_costs ~compare:Int.compare |> Option.value ~default:0
      in
      (* printf "min cost: %d\n" min_cost; *)
      Hashtbl.set
        cost_between_cache
        ~key:(is_numeric_keypad, start, endd, links)
        ~data:min_cost;
      min_cost)
  in
  if Hashtbl.mem cost_cache (is_numeric_keypad, keys, links)
  then Hashtbl.find_exn cost_cache (is_numeric_keypad, keys, links)
  else (
    (* printf "%s \n" (show_ctl (pairwise ('A' :: keys))); *)
    let result =
      List.fold
        (pairwise ('A' :: keys))
        ~init:0
        ~f:(fun acc (ch1, ch2) -> acc + cost_between keypad ch1 ch2 links)
    in
    Hashtbl.set cost_cache ~key:(is_numeric_keypad, keys, links) ~data:result;
    result)
;;

let%test_unit _ = [%test_result: int] (cost numpad [ '0'; '2' ] 1) ~expect:4
let%test_unit _ = [%test_result: int] (cost numpad [ '0'; '2' ] 2) ~expect:12
let%test_unit _ = [%test_result: int] (cost numpad [ '0'; '2' ] 3) ~expect:30
let%test_unit _ = [%test_result: int] (cost numpad [ '2'; 'A' ] 1) ~expect:6
let%test_unit _ = [%test_result: int] (cost numpad [ '2'; 'A' ] 2) ~expect:16
let%test_unit _ = [%test_result: int] (cost numpad [ '0'; '2'; 'A' ] 1) ~expect:7
let%test_unit _ = [%test_result: int] (cost numpad [ '0'; '2'; 'A' ] 2) ~expect:19
let%test_unit _ = [%test_result: int] (cost numpad [ '0'; '2'; '9'; 'A' ] 3) ~expect:68

let complexity code robots =
  let code_num_lst = String.to_list code in
  let code_num_str = String.substr_replace_first code ~pattern:"A" ~with_:"" in
  let code_num = int_of_string code_num_str in
  (* printf "%s %d\n" (show_cl code_num_lst) code_num; *)
  cost numpad code_num_lst (robots + 1) * code_num
;;

let solve_part1 str =
  let codes = Str.split (Str.regexp "\n") str in
  List.fold codes ~init:0 ~f:(fun acc code -> acc + complexity code 2)
;;

let%test_unit _ = [%test_result: int] (solve_part1 "02") ~expect:(2 * 30)
let%test_unit _ = [%test_result: int] (solve_part1 "029A") ~expect:(68 * 29)

(* let%test_unit _ =
   [%test_result: int] (solve_part1 "029A\n980A\n179A\n456A\n379A") ~expect:126384
   ;; *)

let solve_part2 str =
  let codes = Str.split (Str.regexp "\n") str in
  List.fold codes ~init:0 ~f:(fun acc code -> acc + complexity code 25)
;;
