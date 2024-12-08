let input = Utils.read_file_lines "./input.txt"

let () = 
  Puzzle.solve_part1 input
  |> Printf.printf "Part 1 result: %d\n"

let () = 
  Puzzle.solve_part2 input
  |> Printf.printf "Part 2 result: %d\n"