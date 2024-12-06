let file_path = "input.csv"
let csv_file = open_in file_path

let table = 
  Csv.input_all
    (Csv.of_channel csv_file)

let () = close_in csv_file
let () = List.iter (
  fun row ->
    Printf.printf "%s\n" (String.concat "; " row)
) table

let col1, col2 = 
    List.fold_left (fun (acc1, acc2) row -> 
      match row with
      | [col1; col2] -> (col1 :: acc1, col2 :: acc2)
      | [col1] -> 
        let parts = Str.split (Str.regexp " +") col1 in
        (match parts with
        | [col1; col2] -> (col1 :: acc1, col2 :: acc2)
        | _ -> failwith "CSV rows does not have exactly two columns")
      | _ -> failwith "CSV rows does not have exactly two columns"
    ) ([], []) table

let col1_sorted = List.sort compare (List.map int_of_string col1)
let col2_sorted = List.sort compare (List.map int_of_string col2)

let distance = List.map2 (
  fun x y -> Int.abs(x - y)
) col1_sorted col2_sorted

let distance_sum = List.fold_left (+) 0 distance

let () = print_endline (string_of_int distance_sum)
let col2_map = 
  let table = Hashtbl.create 16 in
  List.iter (fun num -> 
    match Hashtbl.find_opt table num with
    | Some count -> Hashtbl.replace table num (count + 1)
    | None -> Hashtbl.add table num 1
  ) col2_sorted;
  table

module IntSet = Set.Make(Int)
let col1_set = IntSet.of_list col1_sorted

let score = IntSet.map (fun num ->
  match Hashtbl.find_opt col2_map num with
  | Some count -> num * count
  | None -> num * 0
) col1_set
let similarity_score = IntSet.fold (+) score 0

let () = Printf.printf "Similarity score: %d" (similarity_score)