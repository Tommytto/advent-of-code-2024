open Core

let read_file_lines file = In_channel.read_lines file
let read_file file = In_channel.read_all file

let rec take n lst =
  match n, lst with
  | 0, _ | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs
;;

let%test _ = equal_list equal_int (take 1 [ 1; 2; 3 ]) [ 1 ]

let rec string_of_int_list lst =
  match lst with
  | [] -> ""
  | [ x ] -> string_of_int x
  | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl
;;

let%test _ = equal_string (string_of_int_list [ 1; 2; 3 ]) "1 2 3"
let print_int_list lst = string_of_int_list lst |> print_endline

let rec string_list_join lst jn =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | hd :: tl -> hd ^ jn ^ string_list_join tl jn
;;

let%test _ =
  equal_string (string_list_join [ "hello"; "world"; "Timur" ] ", ") "hello, world, Timur"
;;

let rec sum_int_list lst =
  match lst with
  | [] -> 0
  | [ x ] -> x
  | hd :: tl -> hd + sum_int_list tl
;;

let%test _ = sum_int_list [ 1; 2; 3 ] = 6

let array_swap arr i j =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp
;;

let%test _ =
  let arr = [| 1; 2; 3 |] in
  array_swap arr 0 2;
  equal_array equal_int arr [| 3; 2; 1 |]
;;

let coord_outside matrix (x, y) =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  x >= rows || x < 0 || y >= columns || y < 0
;;

let copy_matrix matrix = Array.map ~f:Array.copy matrix

let char_matrix_of_string_list str_list =
  str_list |> List.map ~f:String.to_array |> Array.of_list
;;

let char_matrix_of_string str =
  let str_list = Str.split (Str.regexp "\n") str in
  char_matrix_of_string_list str_list
;;

let print_int_matrix m =
  Array.iter
    ~f:(fun row ->
      Array.iter ~f:(fun elem -> Printf.printf "%d " elem) row;
      printf "\n";
      ())
    m
;;

let print_char_matrix m =
  Array.iter
    ~f:(fun row ->
      Array.iter ~f:(fun elem -> Printf.printf "%c " elem) row;
      printf "\n";
      ())
    m
;;

let char_to_int c = int_of_string (Char.to_string c)

let copy_matrix_size matrix init =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  Array.make_matrix ~dimx:rows ~dimy:columns init
;;

let log s = Stdio.printf "%s\n" s

(* Function to create a large PPM image from multiple matrices *)
let write_huge_image filename matrices gap =
  let num_matrices = Array.length matrices in
  let single_height = Array.length matrices.(0) in
  let single_width = Array.length matrices.(0).(0) in
  (* Determine grid size *)
  let grid_size = int_of_float (Float.round_up (sqrt (float_of_int num_matrices))) in
  (* Calculate final image dimensions *)
  let total_height = ((single_height + gap) * grid_size) - gap in
  let total_width = ((single_width + gap) * grid_size) - gap in
  (* Open output file *)
  let oc = Out_channel.create filename in
  (* Write PPM header *)
  Printf.fprintf oc "P3\n";
  Printf.fprintf oc "%d %d\n" total_width total_height;
  Printf.fprintf oc "255\n";
  (* Initialize a white canvas *)
  let canvas =
    Array.make_matrix ~dimx:total_height ~dimy:total_width [| 255; 255; 255 |]
  in
  (* Place matrices into the canvas *)
  let place_matrix matrix x_offset y_offset =
    for i = 0 to Array.length matrix - 1 do
      for j = 0 to Array.length matrix.(i) - 1 do
        let pixel = if matrix.(i).(j) = 1 then [| 0; 0; 0 |] else [| 255; 255; 255 |] in
        canvas.(y_offset + i).(x_offset + j) <- pixel
      done
    done
  in
  (* Place each matrix *)
  let rec place_all_matrices idx row col =
    if idx >= num_matrices
    then ()
    else (
      let x_offset = col * (single_width + gap) in
      let y_offset = row * (single_height + gap) in
      place_matrix matrices.(idx) x_offset y_offset;
      if col + 1 < grid_size
      then place_all_matrices (idx + 1) row (col + 1)
      else place_all_matrices (idx + 1) (row + 1) 0)
  in
  place_all_matrices 0 0 0;
  (* Write canvas to the file *)
  for i = 0 to total_height - 1 do
    for j = 0 to total_width - 1 do
      let pixel = canvas.(i).(j) in
      Printf.fprintf oc "%d %d %d " pixel.(0) pixel.(1) pixel.(2)
    done;
    Printf.fprintf oc "\n"
  done;
  (* Close file *)
  Out_channel.close oc
;;

let explode s = String.to_list s

type point =
  { y : int
  ; x : int
  }

let swap_matrix_elem matrix a b =
  let tmp = matrix.(a.y).(a.x) in
  matrix.(a.y).(a.x) <- matrix.(b.y).(b.x);
  matrix.(b.y).(b.x) <- tmp
;;

open Core

let time f x =
  let start_time = Time_ns.now () in
  let result = f x in
  let end_time = Time_ns.now () in
  let elapsed = Time_ns.diff end_time start_time in
  printf "Execution time: %s\n" (Time_ns.Span.to_string elapsed);
  result
;;

let find_char_pos matrix char =
  let p_opt =
    Array.find_mapi matrix ~f:(fun y row ->
      Array.find_mapi row ~f:(fun x cell ->
        if equal_char cell char then Some (y, x) else None))
  in
  let y, x = Option.value_exn p_opt in
  y, x
;;

let make_pair_combinations lst =
  let rec aux lst =
    match lst with
    | hd :: tl -> List.map tl ~f:(fun item -> hd, item) @ aux tl
    | _ -> []
  in
  aux lst
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (make_pair_combinations [ 1; 2; 3 ])
    ~expect:[ 1, 2; 1, 3; 2, 3 ]
;;
