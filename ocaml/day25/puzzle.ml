(* decompose
   part 1 is look easy
   read the matrix, convert to heights
   after sum key and lock column if it's <= 5 then fit otherwise not
   as soon as we need unique pairs, lets store list of columns in set Set (int list * int list)  or better strings
*)
open Core

let matrix_height matrix =
  let is_lock = equal_char matrix.(0).(0) '#' in
  let omit_row_idx = if is_lock then 0 else Array.length matrix - 1 in
  let result = Array.init (Array.length matrix.(0)) ~f:(fun _ -> 0) in
  let rec aux y x =
    if y = Array.length matrix
    then aux 0 (x + 1)
    else if x = Array.length matrix.(0)
    then ()
    else if y = omit_row_idx
    then aux (y + 1) x
    else (
      let count = if equal_char matrix.(y).(x) '#' then 1 else 0 in
      result.(x) <- result.(x) + count;
      aux (y + 1) x)
  in
  aux 0 0;
  result
;;

let%test_unit _ =
  [%test_result: int array]
    (matrix_height
       (Utils.char_matrix_of_string "#####\n.####\n.####\n.####\n.#.#.\n.#...\n....."))
    ~expect:[| 0; 5; 3; 4; 3 |]
;;

let%test_unit _ =
  [%test_result: int array]
    (matrix_height
       (Utils.char_matrix_of_string ".....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####"))
    ~expect:[| 5; 0; 2; 1; 3 |]
;;

let find_unique_pairs_count matrix_list =
  let keys = List.filter matrix_list ~f:(fun m -> equal_char m.(0).(0) '.') in
  let locks = List.filter matrix_list ~f:(fun m -> equal_char m.(0).(0) '#') in
  let counter = ref 0 in
  List.iter keys ~f:(fun key ->
    let key_pins = matrix_height key in
    List.iter locks ~f:(fun lock ->
      let lock_pins = matrix_height lock in
      let fit =
        Array.for_alli lock_pins ~f:(fun idx pin ->
          let key_pin = key_pins.(idx) in
          if key_pin + pin <= 5 then true else false)
      in
      if fit then counter := !counter + 1));
  !counter
;;

let solve_part1 str =
  let matrices =
    str |> Str.split (Str.regexp "\n\n") |> List.map ~f:Utils.char_matrix_of_string
  in
  find_unique_pairs_count matrices
;;

let solve_part2 _ = -1

let%test_unit _ =
  [%test_result: int]
    (solve_part1
       "#####\n\
        .####\n\
        .####\n\
        .####\n\
        .#.#.\n\
        .#...\n\
        .....\n\n\
        #####\n\
        ##.##\n\
        .#.##\n\
        ...##\n\
        ...#.\n\
        ...#.\n\
        .....\n\n\
        .....\n\
        #....\n\
        #....\n\
        #...#\n\
        #.#.#\n\
        #.###\n\
        #####\n\n\
        .....\n\
        .....\n\
        #.#..\n\
        ###..\n\
        ###.#\n\
        ###.#\n\
        #####\n\n\
        .....\n\
        .....\n\
        .....\n\
        #....\n\
        #.#..\n\
        #.#.#\n\
        #####")
    ~expect:3
;;
