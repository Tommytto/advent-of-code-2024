let read_file_lines file = In_channel.with_open_bin file In_channel.input_lines
let read_file file = In_channel.with_open_bin file In_channel.input_all

let rec take n lst =
  match n, lst with
  | 0, _ | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs
;;

let%test _ = take 1 [ 1; 2; 3 ] = [ 1 ]

let rec string_of_int_list lst =
  match lst with
  | [] -> ""
  | [ x ] -> string_of_int x
  | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl
;;

let%test _ = string_of_int_list [ 1; 2; 3 ] = "1 2 3"
let print_int_list lst = string_of_int_list lst |> print_endline

let rec string_list_join lst jn =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | hd :: tl -> hd ^ jn ^ string_list_join tl jn
;;

let%test _ = string_list_join [ "hello"; "world"; "Timur" ] ", " = "hello, world, Timur"

let rec sum_int_list lst =
  match lst with
  | [] -> 0
  | [ x ] -> x
  | hd :: tl -> hd + sum_int_list tl
;;

let%test _ = sum_int_list [ 1; 2; 3 ] = 6

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let print_int_set set = IntSet.iter (fun v -> Printf.printf "%d," v) set

let print_int_map_set map =
  IntMap.iter
    (fun k v ->
      Printf.printf "key: %d\n" k;
      Printf.printf "values: ";
      print_int_set v;
      Printf.printf "\n------\n")
    map
;;

let array_swap arr i j =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp
;;

let%test _ =
  let arr = [| 1; 2; 3 |] in
  array_swap arr 0 2;
  arr = [| 3; 2; 1 |]
;;

let coord_outside matrix (x, y) =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  x >= rows || x < 0 || y >= columns || y < 0
;;

let copy_matrix matrix = Array.map Array.copy matrix

let char_matrix_of_string_list str_list =
  str_list
  |> List.map String.to_seq
  |> List.map List.of_seq
  |> List.map Array.of_list
  |> Array.of_list
;;

let char_matrix_of_string str =
  let str_list = Str.split (Str.regexp "\n") str in
  char_matrix_of_string_list str_list
;;

let print_matrix m =
  Array.iter
    (fun row ->
      Array.iter (fun elem -> Printf.printf "%d " elem) row;
      print_newline ())
    m
;;

let char_to_int c =
  if c >= '0' && c <= '9'
  then Char.code c - Char.code '0'
  else failwith "Not a digit character"
;;

let copy_matrix_size matrix init =
  let rows = Array.length matrix in
  let columns = Array.length matrix.(0) in
  Array.make_matrix rows columns init
;;

let log s = Stdio.printf "%s\n" s

(* Function to create a large PPM image from multiple matrices *)
let write_huge_image filename matrices gap =
  let num_matrices = Array.length matrices in
  let single_height = Array.length matrices.(0) in
  let single_width = Array.length matrices.(0).(0) in
  (* Determine grid size *)
  let grid_size = int_of_float (ceil (sqrt (float_of_int num_matrices))) in
  (* Calculate final image dimensions *)
  let total_height = ((single_height + gap) * grid_size) - gap in
  let total_width = ((single_width + gap) * grid_size) - gap in
  (* Open output file *)
  let oc = open_out filename in
  (* Write PPM header *)
  Printf.fprintf oc "P3\n";
  Printf.fprintf oc "%d %d\n" total_width total_height;
  Printf.fprintf oc "255\n";
  (* Initialize a white canvas *)
  let canvas = Array.make_matrix total_height total_width [| 255; 255; 255 |] in
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
  close_out oc
;;
