let input = Utils.read_file "./input.txt"
let () = Utils.time Puzzle.solve_part1 input |> Printf.printf "Part 1 result: %d\n"
let () = Utils.time Puzzle.solve_part2 input |> Printf.printf "Part 2 result: %s\n"
