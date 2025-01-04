(* decompose
   1. build graph with Node and neighbours
   2. iterate over all Nodes and
*)

open Core

let build_graph pairs_lst =
  let tbl = Hashtbl.create (module String) in
  List.iter pairs_lst ~f:(fun (a, b) ->
    let set_a = Hashtbl.find_or_add tbl a ~default:(fun _ -> Set.empty (module String)) in
    let set_b = Hashtbl.find_or_add tbl b ~default:(fun _ -> Set.empty (module String)) in
    Hashtbl.set tbl ~key:a ~data:(Set.add set_a b);
    Hashtbl.set tbl ~key:b ~data:(Set.add set_b a));
  tbl
;;

type string_list = string list [@@deriving show]

let rec make_combinations lst n =
  match n, lst with
  | 0, _ -> [ [] ] (* A single combination of size 0 is the empty list *)
  | _, [] -> [] (* No combinations can be made from an empty list *)
  | _, hd :: tl ->
    let with_hd = List.map (make_combinations tl (n - 1)) ~f:(fun comb -> hd :: comb) in
    let without_hd = make_combinations tl n in
    with_hd @ without_hd
;;

let make_combinations_with_hd lst n =
  if List.length lst < n
  then []
  else (
    let hd, tl = List.hd_exn lst, List.tl_exn lst in
    let combs = make_combinations tl (n - 1) in
    List.map combs ~f:(fun c -> hd :: c))
;;

let make_groups_from_lst graph lst n =
  (* printf "list: %s\n" (show_string_list lst); *)
  let combinations = make_combinations_with_hd lst n in
  List.filter combinations ~f:(fun part ->
    (* printf "comb: %s\n" (show_string_list part); *)
    let rec aux lst =
      match lst with
      | hd :: tl ->
        let set =
          Hashtbl.find_or_add graph hd ~default:(fun _ -> Set.empty (module String))
        in
        let valid = List.for_all tl ~f:(fun n -> Set.mem set n) in
        if valid then aux tl else false
      | _ -> true
    in
    aux part)
;;

let make_groups ?(prefix = "") graph n =
  let result_set = Hash_set.create (module String) in
  Hashtbl.iteri graph ~f:(fun ~key ~data ->
    let valid = if String.is_empty prefix then true else String.is_prefix key ~prefix in
    if valid
    then (
      let n_lets = make_groups_from_lst graph (key :: Set.to_list data) n in
      List.iter n_lets ~f:(fun group ->
        let sorted = List.sort group ~compare:compare_string |> List.to_array in
        let k = String.concat_array sorted ~sep:"," in
        Hash_set.add result_set k)));
  Hash_set.to_list result_set
;;

let str_to_pairs str =
  let lst = Str.split (Str.regexp "\n") str in
  let lst =
    List.map lst ~f:(fun str ->
      let parts = Str.split (Str.regexp "-") str in
      List.nth_exn parts 0, List.nth_exn parts 1)
  in
  lst
;;

let solve_part1 str =
  let pairs = str_to_pairs str in
  let graph = build_graph pairs in
  let groups = make_groups ~prefix:"t" graph 3 in
  List.length groups
;;

let solve_part2 str =
  let pairs = str_to_pairs str in
  let graph = build_graph pairs in
  let rec binary_search left right =
    if left >= right
    then left
    else (
      let mid = left + ((right - left) / 2) in
      let groups = make_groups graph mid in
      if List.length groups = 1
      then mid
      else if List.length groups = 0
      then binary_search left (mid - 1)
      else binary_search (mid + 1) right)
  in
  let n = binary_search 0 500 in
  let groups = make_groups graph n in
  List.hd_exn groups
;;

let%test_unit _ =
  [%test_result: int]
    (solve_part1
       "kh-tc\n\
        qp-kh\n\
        de-cg\n\
        ka-co\n\
        yn-aq\n\
        qp-ub\n\
        cg-tb\n\
        vc-aq\n\
        tb-ka\n\
        wh-tc\n\
        yn-cg\n\
        kh-ub\n\
        ta-co\n\
        de-co\n\
        tc-td\n\
        tb-wq\n\
        wh-td\n\
        ta-ka\n\
        td-qp\n\
        aq-cg\n\
        wq-ub\n\
        ub-vc\n\
        de-ta\n\
        wq-aq\n\
        wq-vc\n\
        wh-yn\n\
        ka-de\n\
        kh-ta\n\
        co-tc\n\
        wh-qp\n\
        tb-vc\n\
        td-yn")
    ~expect:7
;;

let%test_unit _ =
  [%test_result: string]
    (solve_part2
       "kh-tc\n\
        qp-kh\n\
        de-cg\n\
        ka-co\n\
        yn-aq\n\
        qp-ub\n\
        cg-tb\n\
        vc-aq\n\
        tb-ka\n\
        wh-tc\n\
        yn-cg\n\
        kh-ub\n\
        ta-co\n\
        de-co\n\
        tc-td\n\
        tb-wq\n\
        wh-td\n\
        ta-ka\n\
        td-qp\n\
        aq-cg\n\
        wq-ub\n\
        ub-vc\n\
        de-ta\n\
        wq-aq\n\
        wq-vc\n\
        wh-yn\n\
        ka-de\n\
        kh-ta\n\
        co-tc\n\
        wh-qp\n\
        tb-vc\n\
        td-yn")
    ~expect:"co,de,ka,ta"
;;
