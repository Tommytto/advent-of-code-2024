let reports_to_check =
  let input = Puzzle.read_file "./input.txt" in
  input
  |> List.map (
    fun row -> 
      let lst = List.map int_of_string (String.split_on_char ' ' row) in
      lst
  )
  |> List.map Array.of_list

let safe_reports_v1 = 
  reports_to_check
  |> List.filter Puzzle.valid_report

let safe_reports_v2 = 
  reports_to_check
  |> List.filter Puzzle.valid_report_v2

let () = Printf.printf "Safe reports v1: %d\n" (List.length safe_reports_v1)
let () = Printf.printf "Safe reports v2: %d\n" (List.length safe_reports_v2)