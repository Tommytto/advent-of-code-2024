let read_file_lines file = In_channel.with_open_bin file In_channel.input_lines
let read_file file = In_channel.with_open_bin file In_channel.input_all

let rec take n lst = 
  match n, lst with
  | 0, _ | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs

let%test _ = take 1 [1; 2; 3] = [1]

let rec string_of_int_list lst =
  match lst with
    | [] -> ""
    | [x] -> string_of_int x
    | hd :: tl -> string_of_int hd ^ " " ^ string_of_int_list tl

let%test _ = string_of_int_list [1; 2; 3] = "1 2 3"

let print_int_list lst = 
  string_of_int_list lst
  |> print_endline

let rec string_list_join lst jn = 
  match lst with
  | [] -> ""
  | [x] -> x
  | hd :: tl -> hd ^ jn ^ string_list_join tl jn

let%test _ = string_list_join ["hello"; "world"; "Timur"] ", " = "hello, world, Timur"

let rec sum_int_list lst =
  match lst with
  | [] -> 0
  | [x] -> x
  | hd :: tl -> hd + sum_int_list tl

let%test _ = sum_int_list [1; 2; 3] = 6

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)
let print_int_set set = 
  IntSet.iter (fun v ->
    Printf.printf "%d," v;
  ) set

let print_int_map_set map =
  IntMap.iter (fun k v -> 
    Printf.printf "key: %d\n" k;
    Printf.printf "values: ";
    print_int_set v;
    Printf.printf "\n------\n";
  ) map

let array_swap arr i j =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp;
  arr

let%test _ = array_swap [|1;2;3|] 0 2 = [|3;2;1|]