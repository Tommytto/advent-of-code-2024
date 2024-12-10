(* decompose
  + - decode_dense_format str -> int array
  - 2 pointers rec function left -> dot, right -> num and swap repeat until left >= right point
  - calc result
*)

let explode s = List.init (String.length s) (String.get s)

let rec n_times ?(acc=[]) v count = 
  match count with
  | 0 -> acc
  | _ -> n_times ~acc:(v :: acc) v (count - 1)

let decode_dense str = 
  let num_list = 
    str
    |> explode
    |> List.map (String.make 1)
    |> List.map int_of_string
  in

  let rec aux lst i acc (is_file: bool) =
    match lst with
    | hd :: tl -> 
      let n_times_list = 
        match is_file with
        | true -> (n_times i hd)
        | false -> (n_times (-1) hd)
      in

      let new_acc = Array.append acc (Array.of_list n_times_list) in

      let new_i = if is_file then i+1 else i in
      aux tl new_i new_acc (not is_file)
    | [] -> acc
  in

  aux num_list 0 [||] true

let%test _ = decode_dense "12345" = [|0; -1; -1; 1; 1; 1; -1; -1; -1; -1; 2; 2; 2; 2; 2; |]

let make_groups_pos arr =
  let rec aux left right groups = 
    if right = Array.length arr then groups 
    else
      let is_new_group = (right+1) = Array.length arr || arr.(right+1) != arr.(left) in
      let new_groups = 
        if is_new_group then
            Array.append groups [|(arr.(left), (left, right+1))|]
        else groups
      in
      
      if is_new_group then
        aux (right+1) (right+1) new_groups
      else
        aux left (right+1) new_groups
  in

  aux 0 0 [||]

let%test _ = make_groups_pos [|0; -1; -1; 1; 1; 1; -1; -1; -1; -1; 2; 2; 2; 2; 2; |] = [|
  (0, (0, 1));
  (-1, (1, 3));
  (1, (3, 6));
  (-1, (6, 10));
  (2, (10, 15));
|]

let get_val_group_length start arr step = 
  let rec aux i = 
    if i = Array.length arr || arr.(i) != arr.(start) then Int.abs (i - start)
    else
      aux (i+step)
  in

  aux start

let%test _ = get_val_group_length 0 [| 1; 1; 2; 2|] 1 = 2
let%test _ = get_val_group_length 3 [| 1; 1; 2; 2|] (-1) = 2

let move_left arr =
  let rec aux left right =
    if left >= right then ()
    else
      let left_value, right_value = arr.(left), arr.(right) in

      if left_value != -1 then aux (left+1) right
      else if right_value = -1 then aux left (right-1)
      else begin
        Utils.array_swap arr left right;

        aux (left+1) (right-1)
      end
  in
  
  aux 0 (Array.length arr - 1)

let%test _ = 
  let arr = [|0; -1; -1; 1; 1; 1; -1; -1; -1; -1; 2; 2; 2; 2; 2; |] in
  move_left arr;
  arr = [|0; 2; 2; 1; 1; 1; 2; 2; 2; -1; -1; -1; -1; -1; -1;|]

let swap_n arr left right n =
  let rec aux left right n =
    if n = 0 then ()
    else begin
      Utils.array_swap arr left right;
      aux (left+1) (right-1) (n-1)
    end
  in

  aux left right n

let%test _ = 
  let arr = [|1;2;3;4;5|] in
  swap_n arr 0 4 2;
  arr = [|5;4;3;2;1|]

let find_first_n_free_before ?(before = Int.max_int) arr n =
  let arr_n = Array.length arr in
  let rec aux c1 c2 =
    if c2 - c1 = n then Some(c1,c2)
    else if c2 = arr_n || c2 >= before then None
    else if arr.(c1) != -1 || arr.(c2) != -1 then aux (c2+1) (c2+1)
    else aux c1 (c2+1)
  in

  aux 0 0

let%test _= find_first_n_free_before [|1;1;-1;-1;2;2;-1;-1;-1|] 3 = Some(6, 9)
let%test _ = find_first_n_free_before [|0; 0; -1; -1; -1; 1; 1; 1; -1; -1; -1; 2; -1;|] 4 = None

let move_left_by_blocks arr =
  let rec aux right right_start =
    if right = 0 then ()
    else if arr.(right_start) = -1 then aux (right_start-1) (right_start-1)
    else if arr.(right) = arr.(right_start) then aux (right-1) (right_start)
    else 
      let group_size = right_start - right in
      match find_first_n_free_before ~before:(right + 1) arr group_size with
      | Some(free_start, _) -> begin
          swap_n arr free_start right_start group_size;
          aux right right
        end
      | None -> aux right right
  in

  aux (Array.length arr - 1) (Array.length arr - 1)
  
let checksum arr =
  let rec aux i =
    if i = Array.length arr then 0
    else if arr.(i) = -1 then aux (i+1)
    else arr.(i) * i + aux (i+1)
  in

  aux 0

let%test _ = checksum [|0; 1; 2; 4|] = 17

let solve_part1 str =
  let num_arr = decode_dense str in
  move_left num_arr;
  checksum num_arr

let%test _ = solve_part1 "2333133121414131402" = 1928

let solve_part2 str =
  let num_arr = decode_dense str in
  move_left_by_blocks num_arr;
  checksum num_arr
  
let%test _ = solve_part2 "2333133121414131402" = 2858
