(* decompose
   - hashtable with current values
   - apply operations
   - iterate over hashtable and find starting with z
   - sort
   - make a number

   part2

   so we need to find a list of outputs which have been swapped
   we can do it by finding left operator x y ancestors and right operator ancestors, after that we have a list of x,y used in operation,
   if any of their index is different then start target then we found it

   hmm i realized that if we found b + a = z0 and ancestor is x1 y1 it doesnt mean that z0 ouput is incorrect, may be a or b output is incorrent

   do we really can swap first 8 and check everything, we have 200 operations, so we can s

   we still can find bad z outputs, so we get all operands involved into bad things
   after we can swap first two, run program and check if its getting better
   getting better means that after swap of two elemnts, last z is correct

   why then do we need really search for bad Z's

   is that possible that swap fixes one z and breaks another, probably yes, then we need

   can we rely just on count of good Z's, we swap two and check how many Z's were before and how many now, I suppose we can try
*)

open Core
open Re

type op =
  { left : string
  ; right : string
  ; operator : string
  ; target : string
  }
[@@deriving show]

type string_list = string list [@@deriving show]

let parse_op input =
  (* Compile the regex *)
  let regex = Re.Perl.compile_pat "(.*?) (.*?) (.*?) -> (.*)" in
  match Re.exec_opt regex input with
  | Some groups ->
    let g1 = Re.Group.get groups 1 in
    let g2 = Re.Group.get groups 2 in
    let g3 = Re.Group.get groups 3 in
    let g4 = Re.Group.get groups 4 in
    { left = g1; operator = g2; right = g3; target = g4 }
  | None -> failwith "bad operation"
;;

let wire_id str = Str.last_chars str 2

let rec goes_to ?(deep = 0) target operations =
  if deep = 5
  then ""
  else (
    let op =
      List.find operations ~f:(fun op ->
        equal_string target op.left || equal_string target op.right)
    in
    match op with
    | Some op ->
      if String.is_prefix op.target ~prefix:"z"
      then op.target
      else goes_to ~deep:(deep + 1) op.target operations
    | None -> "")
;;

let parse_input str =
  let values_part, operations_part =
    Str.split (Str.regexp "\n\n") str
    |> fun lst ->
    ( List.nth_exn lst 0 |> Str.split (Str.regexp "\n")
    , List.nth_exn lst 1 |> Str.split (Str.regexp "\n") )
  in
  let values_table = Hashtbl.create (module String) in
  List.iter values_part ~f:(fun str ->
    let key, data =
      Str.split (Str.regexp ":") str
      |> fun lst ->
      let key = List.nth_exn lst 0 in
      let data =
        List.nth_exn lst 1
        |> String.substr_replace_all ~pattern:" " ~with_:""
        |> int_of_string
      in
      key, data
    in
    Hashtbl.set values_table ~key ~data);
  let operations = List.map operations_part ~f:parse_op in
  let xy_operations =
    List.filter operations ~f:(fun op ->
      String.is_prefix op.left ~prefix:"x" && String.is_prefix op.right ~prefix:"y")
  in
  List.iter xy_operations ~f:(fun op ->
    printf "%s goes to %s\n" (show_op op) (goes_to op.left operations));
  let target_table = Hashtbl.create (module String) in
  List.iter operations ~f:(fun op -> Hashtbl.set target_table ~key:op.target ~data:op);
  List.sort operations ~compare:(fun o1 o2 ->
    match compare_string o1.operator o2.operator with
    | 0 -> compare_string o1.left o2.left
    | cmp -> cmp)
  |> List.iter ~f:(fun op -> printf "%s\n" (show_op op));
  values_table, target_table
;;

let bit_list_to_int lst =
  let rec aux lst i =
    match lst with
    | hd :: tl -> (Int.pow 2 i * hd) + aux tl (i + 1)
    | [] -> 0
  in
  aux (List.rev lst) 0
;;

let apply_op left right operator =
  match operator with
  | "AND" -> Int.bit_and left right
  | "XOR" -> Int.bit_xor left right
  | "OR" -> Int.bit_or left right
  | "" | _ -> failwith "unknown operation"
;;

let run_operations values_table target_table =
  let rec calc_operation target =
    if Hashtbl.mem values_table target
    then ()
    else (
      let operation = Hashtbl.find_exn target_table target in
      let left_calculated = Hashtbl.mem values_table operation.left in
      let right_calculated = Hashtbl.mem values_table operation.right in
      if not left_calculated then calc_operation operation.left;
      if not right_calculated then calc_operation operation.right;
      let left = Hashtbl.find_exn values_table operation.left in
      let right = Hashtbl.find_exn values_table operation.right in
      let result = apply_op left right operation.operator in
      Hashtbl.set values_table ~key:target ~data:result)
  in
  Hashtbl.iter_keys target_table ~f:(fun key ->
    if String.is_prefix key ~prefix:"z" then calc_operation key else ());
  Hashtbl.to_alist values_table
  |> List.filter ~f:(fun (key, _) -> String.is_prefix key ~prefix:"z")
  |> (fun lst ->
       List.iter lst ~f:(fun (key, value) ->
         let z_id = wire_id key in
         let x_id = "x" ^ z_id in
         let y_id = "y" ^ z_id in
         printf
           "x=%d y=%d z=%d calc_z=%d"
           (Hashtbl.find_exn values_table x_id)
           (Hashtbl.find_exn values_table y_id)
           value
           (Int.bit_or
              (Hashtbl.find_exn values_table x_id)
              (Hashtbl.find_exn values_table y_id)));
       lst)
  |> List.sort ~compare:(fun (key1, _) (key2, _) -> compare_string key1 key2)
  |> List.map ~f:(fun (_, data) -> data)
  |> List.rev
  |> bit_list_to_int
;;

let find_ancestors target_table target =
  let parents_set = Hash_set.create (module String) in
  let rec aux target =
    if Hash_set.mem parents_set target
    then ()
    else (
      Hash_set.add parents_set target;
      if Hashtbl.mem target_table target
      then (
        let op = Hashtbl.find_exn target_table target in
        aux op.left;
        aux op.right))
  in
  aux target;
  Hash_set.to_list parents_set
;;

let is_valid_ancestors target_id parents =
  let xy_parents =
    List.filter parents ~f:(fun p ->
      String.is_prefix p ~prefix:"y" || String.is_prefix p ~prefix:"x")
  in
  let x_parents = List.filter xy_parents ~f:(fun p -> String.is_prefix p ~prefix:"x") in
  let y_parents = List.filter xy_parents ~f:(fun p -> String.is_prefix p ~prefix:"y") in
  if List.length x_parents > 1 || List.length y_parents > 1
  then false
  else (
    let x_parent_id = List.hd_exn x_parents |> wire_id in
    let y_parent_id = List.hd_exn x_parents |> wire_id in
    equal_string x_parent_id target_id && equal_string y_parent_id target_id)
;;

let part2_example =
  "x00: 0\n\
   x01: 1\n\
   x02: 0\n\
   x03: 1\n\
   x04: 0\n\
   x05: 1\n\
   y00: 0\n\
   y01: 0\n\
   y02: 1\n\
   y03: 1\n\
   y04: 0\n\
   y05: 1\n\n\
   x00 AND y00 -> z05\n\
   x01 AND y01 -> z02\n\
   x02 AND y02 -> z01\n\
   x03 AND y03 -> z03\n\
   x04 AND y04 -> z04\n\
   x05 AND y05 -> z00"
;;

let find_invalid_outputs_list target_table =
  Hashtbl.fold target_table ~init:[] ~f:(fun ~key ~data:_ acc ->
    if not (String.is_prefix key ~prefix:"z")
    then acc
    else (
      let target_id = wire_id key in
      let parents = find_ancestors target_table key in
      if is_valid_ancestors target_id parents
      then acc
      else (
        let non_base_parents =
          List.filter parents ~f:(fun p ->
            (not (String.is_prefix p ~prefix:"x")) && not (String.is_prefix p ~prefix:"y"))
        in
        (key, non_base_parents) :: acc)))
;;

let%test_unit _ =
  let _, target_table = parse_input part2_example in
  let invalid_lst = find_invalid_outputs_list target_table in
  [%test_result: int] (List.length invalid_lst) ~expect:4
;;

let tbl_swap table a b =
  let op_a = Hashtbl.find_exn table a in
  let op_b = Hashtbl.find_exn table b in
  Hashtbl.set table ~key:a ~data:op_b;
  Hashtbl.set table ~key:b ~data:op_a;
  ()
;;

let find_swapped_output target_table invalid_all =
  if List.length invalid_all = 0
  then "", ""
  else (
    (* printf "invalid_keys: %s\n" (show_string_list invalid_all); *)
    let best_key = ref (List.hd_exn invalid_all) in
    let best_key_value = ref (List.length invalid_all) in
    let rec aux lst =
      match lst with
      | k1 :: k2 :: tl ->
        let op1 = Hashtbl.find_exn target_table k1 in
        let op2 = Hashtbl.find_exn target_table k2 in
        Hashtbl.set target_table ~key:k1 ~data:op2;
        Hashtbl.set target_table ~key:k2 ~data:op1;
        let tmp_result = find_invalid_outputs_list target_table |> List.length in
        Hashtbl.set target_table ~key:k1 ~data:op1;
        Hashtbl.set target_table ~key:k2 ~data:op2;
        if tmp_result < !best_key_value
        then (
          best_key_value := tmp_result;
          best_key := k2);
        aux (k1 :: tl)
      | _ -> ()
    in
    (* printf "best_key: %s %s \n" (List.hd_exn invalid_all) !best_key; *)
    aux invalid_all;
    List.hd_exn invalid_all, !best_key)
;;

(* let%test_unit _ =
   [%test_result: string]
   (snd (find_swapped_output (snd (parse_input part2_example))))
   ~expect:"z02"
   ;; *)

let find_pairs target_table =
  printf "find pairs\n";
  let rec aux invalid_all result =
    printf "List length: %d\n" (List.length invalid_all);
    Out_channel.flush stdout;
    if List.length invalid_all = 0
    then result
    else (
      let prev_best_key, new_best_key = find_swapped_output target_table invalid_all in
      if equal_string prev_best_key new_best_key
      then aux (List.tl_exn invalid_all) result
      else (
        tbl_swap target_table prev_best_key new_best_key;
        let invalid_outputs = find_invalid_outputs_list target_table in
        let invalid_all = List.map invalid_outputs ~f:snd |> List.concat in
        aux invalid_all ((prev_best_key, new_best_key) :: result)))
  in
  let invalid_outputs = find_invalid_outputs_list target_table in
  List.iter invalid_outputs ~f:(fun out ->
    printf "out: %s %s \n" (fst out) (show_string_list (snd out)));
  printf "finihs out\n";
  let invalid_all = List.map invalid_outputs ~f:snd |> List.concat in
  let pairs = aux invalid_all [] in
  List.map pairs ~f:(fun p -> [ fst p; snd p ])
  |> List.concat
  |> List.sort ~compare:compare_string
;;

let%test_unit _ =
  [%test_result: string list]
    (find_pairs (snd (parse_input part2_example)))
    ~expect:[ "z00"; "z01"; "z02"; "z05" ]
;;

let solve_part1 str =
  let table, operations = parse_input str in
  run_operations table operations
;;

let solve_part2 _ = ""
(* let _, target_table = parse_input str in
   find_pairs target_table |> String.concat ~sep:"," *)

let%test_unit _ =
  [%test_result: int]
    (solve_part1
       "x00: 1\n\
        x01: 1\n\
        x02: 1\n\
        y00: 0\n\
        y01: 1\n\
        y02: 0\n\n\
        x00 AND y00 -> z00\n\
        x01 XOR y01 -> z01\n\
        x02 OR y02 -> z02")
    ~expect:4
;;

let%test_unit _ =
  [%test_result: int]
    (solve_part1
       "x00: 1\n\
        x01: 0\n\
        x02: 1\n\
        x03: 1\n\
        x04: 0\n\
        y00: 1\n\
        y01: 1\n\
        y02: 1\n\
        y03: 1\n\
        y04: 1\n\n\
        ntg XOR fgs -> mjb\n\
        y02 OR x01 -> tnw\n\
        kwq OR kpj -> z05\n\
        x00 OR x03 -> fst\n\
        tgd XOR rvg -> z01\n\
        vdt OR tnw -> bfw\n\
        bfw AND frj -> z10\n\
        ffh OR nrd -> bqk\n\
        y00 AND y03 -> djm\n\
        y03 OR y00 -> psh\n\
        bqk OR frj -> z08\n\
        tnw OR fst -> frj\n\
        gnj AND tgd -> z11\n\
        bfw XOR mjb -> z00\n\
        x03 OR x00 -> vdt\n\
        gnj AND wpb -> z02\n\
        x04 AND y00 -> kjc\n\
        djm OR pbm -> qhw\n\
        nrd AND vdt -> hwm\n\
        kjc AND fst -> rvg\n\
        y04 OR y02 -> fgs\n\
        y01 AND x02 -> pbm\n\
        ntg OR kjc -> kwq\n\
        psh XOR fgs -> tgd\n\
        qhw XOR tgd -> z09\n\
        pbm OR djm -> kpj\n\
        x03 XOR y03 -> ffh\n\
        x00 XOR y04 -> ntg\n\
        bfw OR bqk -> z06\n\
        nrd XOR fgs -> wpb\n\
        frj XOR qhw -> z04\n\
        bqk OR frj -> z07\n\
        y03 OR x01 -> nrd\n\
        hwm AND bqk -> z03\n\
        tgd XOR rvg -> z12\n\
        tnw OR pbm -> gnj")
    ~expect:2024
;;

(* qqp
   pbv
   fbq
   z23
   z16
   z36

   x15 y15 output -> z17, figure out why
*)
