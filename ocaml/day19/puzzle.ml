(* decompose
   dfs
   we can make a recursive function (is_pattern_possible) which takes pattern and answer is that possible or not
   inside this function we take every sequnce of letter

   0..1 && is_pattern_possible 1..6
   0..2 && && is_pattern_possible 2..6 and so on
   0..3
   0..4
   0..5
   0..6
   design: brwrr
*)

open Core

let is_design_possible design towels =
  let towels_set = Set.of_list (module String) towels in
  let rec dfs design =
    let n = String.length design in
    let rec aux take_n =
      if equal_string design ""
      then true
      else if take_n > n
      then false
      else (
        let substr = String.sub design ~pos:0 ~len:take_n in
        let rest =
          if take_n >= n then "" else String.sub design ~pos:take_n ~len:(n - take_n)
        in
        if Set.mem towels_set substr && dfs rest then true else aux (take_n + 1))
    in
    aux 1
  in
  dfs design
;;

let get_design_variations_count design towels =
  let towels_set = Set.of_list (module String) towels in
  let count_cache = Hashtbl.create (module String) in
  let rec dfs design =
    if Hashtbl.mem count_cache design
    then Hashtbl.find_exn count_cache design
    else (
      let n = String.length design in
      let rec aux take_n count =
        if equal_string design ""
        then count
        else if take_n > n
        then count
        else (
          let substr = String.sub design ~pos:0 ~len:take_n in
          let rest =
            if take_n >= n then "" else String.sub design ~pos:take_n ~len:(n - take_n)
          in
          let rest_variations_count = dfs rest in
          Hashtbl.set count_cache ~key:rest ~data:rest_variations_count;
          if Set.mem towels_set substr
          then
            if equal_string rest ""
            then aux (take_n + 1) (count + 1)
            else aux (take_n + 1) (count + rest_variations_count)
          else aux (take_n + 1) count)
      in
      aux 1 0)
  in
  dfs design
;;

let%test_unit _ =
  let result =
    get_design_variations_count "brwrr" [ "r"; "wr"; "b"; "g"; "bwu"; "rb"; "gb"; "br" ]
  in
  [%test_result: int] result ~expect:2
;;

let%test_unit _ =
  [%test_result: bool]
    (is_design_possible "brwrr" [ "r"; "wr"; "b"; "g"; "bwu"; "rb"; "gb"; "br" ])
    ~expect:true
;;

let str_to_input str =
  let parts = Str.split (Str.regexp "\n\n") str in
  let towels = Str.split (Str.regexp ", ") (List.nth_exn parts 0) in
  let designs = Str.split (Str.regexp "\n") (List.nth_exn parts 1) in
  towels, designs
;;

let%test_unit _ =
  [%test_result: string list * string list]
    (str_to_input "r, br\n\nbrwrr\nbggr\n")
    ~expect:([ "r"; "br" ], [ "brwrr"; "bggr" ])
;;

let count_possible_designs towels designs =
  List.filter designs ~f:(fun d -> is_design_possible d towels) |> List.length
;;

let solve_part1 str =
  let towels, designs = str_to_input str in
  count_possible_designs towels designs
;;

let%test_unit _ =
  let result =
    solve_part1
      "r, wr, b, g, bwu, rb, gb, br\n\n\
       brwrr\n\
       bggr\n\
       gbbr\n\
       rrbgbr\n\
       ubwu\n\
       bwurrg\n\
       brgr\n\
       bbrgwb"
  in
  [%test_result: int] result ~expect:6
;;

let solve_part2 str =
  let towels, designs = str_to_input str in
  designs
  |> List.map ~f:(fun d -> get_design_variations_count d towels)
  |> List.fold ~init:0 ~f:( + )
;;

let%test_unit _ =
  let result =
    solve_part2
      "r, wr, b, g, bwu, rb, gb, br\n\n\
       brwrr\n\
       bggr\n\
       gbbr\n\
       rrbgbr\n\
       ubwu\n\
       bwurrg\n\
       brgr\n\
       bbrgwb"
  in
  [%test_result: int] result ~expect:16
;;
