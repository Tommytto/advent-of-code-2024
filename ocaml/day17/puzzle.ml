(* decompose
   i think we can start with defining type of the program
   register_a,b,c
   program as int list
   cursor
   opcode
   operand?

   parse input to this program and I think we need some methods
   run_next - takes next pair of (opcode, operand) and returns new state of the program
*)

open Core

type program =
  { register_A : int
  ; register_B : int
  ; register_C : int
  ; cursor : int
  ; commands : int array
  ; program_output : int list
  }

let get_current_instruction program =
  if program.cursor >= Array.length program.commands
  then failwith "Program finished"
  else (
    let opcode = program.commands.(program.cursor) in
    let operand = program.commands.(program.cursor + 1) in
    opcode, operand)
;;

let combo_operand_value operand program =
  match operand with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> program.register_A
  | 5 -> program.register_B
  | 6 -> program.register_C
  | _ -> failwith "Forbidden operand"
;;

let handle_opcode opcode operand program =
  let prev_cursor = program.cursor in
  let new_program =
    match opcode with
    | 0 ->
      { program with
        register_A = program.register_A / Int.pow 2 (combo_operand_value operand program)
      }
    | 1 -> { program with register_B = Int.( lxor ) program.register_B operand }
    | 2 -> { program with register_B = combo_operand_value operand program % 8 }
    | 3 -> if program.register_A = 0 then program else { program with cursor = operand }
    | 4 ->
      { program with register_B = Int.( lxor ) program.register_B program.register_C }
    | 5 ->
      { program with
        program_output =
          (combo_operand_value operand program % 8) :: program.program_output
      }
    | 6 ->
      { program with
        register_B = program.register_A / Int.pow 2 (combo_operand_value operand program)
      }
    | 7 ->
      { program with
        register_C = program.register_A / Int.pow 2 (combo_operand_value operand program)
      }
    | _ -> failwith "Unknown operation code"
  in
  if Int.equal prev_cursor new_program.cursor
  then { new_program with cursor = new_program.cursor + 2 }
  else new_program
;;

let run_next p =
  let opcode, operand = get_current_instruction p in
  let new_program = handle_opcode opcode operand p in
  new_program
;;

let run_program start_program =
  let rec aux program =
    if program.cursor < Array.length program.commands
    then (
      let new_program = run_next program in
      aux new_program)
    else program
  in
  aux start_program
;;

let test_program =
  { register_A = 729
  ; register_B = 0
  ; register_C = 0
  ; commands = [| 0; 1; 5; 4; 3; 0 |]
  ; cursor = 0
  ; program_output = []
  }
;;

let expected_output = [ 4; 6; 3; 5; 6; 3; 5; 2; 1; 0 ]

type int_list = int list [@@deriving show, sexp, equal]

let%test _ =
  let updated_program = run_program test_program in
  equal_int_list (List.rev updated_program.program_output) expected_output
;;

let extract_and_parse_int_exn s =
  let re = Str.regexp "[0-9]+" in
  let _ = Str.search_forward re s 0 in
  int_of_string (Str.matched_string s)
;;

let str_to_program str =
  let parts = Str.split (Str.regexp "\n\n") str in
  let p1, p2 = List.nth_exn parts 0, List.nth_exn parts 1 in
  let registers_strs = Str.split (Str.regexp "\n") p1 in
  let a, b, c =
    List.foldi registers_strs ~init:(0, 0, 0) ~f:(fun idx (a, b, c) r_str ->
      match idx with
      | 0 -> extract_and_parse_int_exn r_str, b, c
      | 1 -> a, extract_and_parse_int_exn r_str, c
      | 2 -> a, b, extract_and_parse_int_exn r_str
      | _ -> failwith "unknown register")
  in
  let p2_parts = Str.split (Str.regexp "Program: ") p2 in
  let commands_str = List.nth_exn p2_parts 0 in
  let commands =
    Str.split (Str.regexp ",") commands_str |> List.map ~f:int_of_string |> Array.of_list
  in
  { register_A = a
  ; register_B = b
  ; register_C = c
  ; program_output = []
  ; cursor = 0
  ; commands
  }
;;

let get_program_output program =
  List.rev program.program_output |> List.map ~f:string_of_int |> String.concat ~sep:","
;;

let bit_equation a = Int.( lxor ) (a % 8) (a / Int.pow 2 (Int.( lxor ) (a % 8) 7))

let int_to_bit_list bits_count n =
  let rec aux n bits_left =
    if bits_left = 0
    then []
    else (
      let bit = 1 land n in
      bit :: aux (n lsr 1) (bits_left - 1))
  in
  List.rev (aux n bits_count)
;;

let ss str a =
  let program = str_to_program str in
  let program = { program with register_A = a } in
  let completed_program = run_program program in
  get_program_output completed_program
;;

let bit_list_to_int lst =
  let rec aux lst i =
    match lst with
    | hd :: tl -> (Int.pow 2 i * hd) + aux tl (i + 1)
    | [] -> 0
  in
  aux (List.rev lst) 0
;;

let b =
  { register_A = 121024
  ; register_B = 0
  ; register_C = 0
  ; cursor = 0
  ; program_output = []
  ; commands = [| 0; 3; 5; 4; 3; 0 |]
  }
;;

let bit_3_variants =
  [ [ 0; 0; 0 ]
  ; [ 0; 0; 1 ]
  ; [ 0; 1; 0 ]
  ; [ 0; 1; 1 ]
  ; [ 1; 0; 0 ]
  ; [ 1; 0; 1 ]
  ; [ 1; 1; 0 ]
  ; [ 1; 1; 1 ]
  ]
;;

let find_3_bit program prev i =
  let pos = Int.abs (i - (Array.length program.commands - 1)) in
  let target_output = program.commands.(i) in
  let rec aux variants result_variants =
    match variants with
    | variant :: rest_variants ->
      let a_candidate_bits = prev @ variant in
      let a_candidate_int = bit_list_to_int a_candidate_bits in
      let test_program = { program with register_A = a_candidate_int } in
      let run_program_result = run_program test_program in
      if List.length run_program_result.program_output - 1 >= pos
         && (Array.of_list run_program_result.program_output).(pos) = target_output
      then aux rest_variants (a_candidate_bits :: result_variants)
      else aux rest_variants result_variants
    | _ -> result_variants
  in
  aux bit_3_variants []
;;

let find_a program =
  let total_size = Array.length program.commands in
  let rec dfs i all_variants =
    (* printf "i: %d\n" i; *)
    if i <= -1
    then all_variants
    else (
      let new_all_variants =
        List.fold all_variants ~init:[] ~f:(fun acc prev ->
          let current_variants = find_3_bit program prev i in
          acc @ current_variants)
      in
      dfs (i - 1) new_all_variants)
  in
  let all_variants = dfs (total_size - 1) bit_3_variants in
  (* List.iter all_variants ~f:(fun v -> printf "result variant: %s\n" (show_int_list v)); *)
  let all_variants = List.map all_variants ~f:bit_list_to_int in
  List.min_elt all_variants ~compare:Int.compare
;;

let solve_part1 str =
  let program = str_to_program str in
  let completed_program = run_program program in
  get_program_output completed_program
;;

let%test _ =
  let result =
    solve_part1 "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
  in
  equal_string result "4,6,3,5,6,3,5,2,1,0"
;;

let solve_part2 str =
  let program = str_to_program str in
  Option.value_exn (find_a program)
;;

let%test _ =
  let result =
    solve_part2 "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"
  in
  result = 117440
;;
