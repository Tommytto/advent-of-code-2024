let valid_report arr : bool =
  let n = Array.length arr in
  let is_decreasing = ref false in
  let rec aux i = 
    if i >= n - 1 then true
    else
      let diff = arr.(i + 1) - arr.(i) in
      if i = 0 then 
        is_decreasing := diff < 0;
    
      let abs_diff = Int.abs diff in
      if !is_decreasing && diff >= 0 then false
      else if not !is_decreasing && diff <= 0 then false
      else if abs_diff < 1 || abs_diff > 3 then false 
      else aux (i + 1)
  in
  aux 0

let arr_exclude arr ex =
  let len = Array.length arr in
  Array.init (len-1) (
    fun (i) ->
      if i < ex then arr.(i)
      else arr.(i+1)
  )

let valid_report_v2 arr : bool =
  let is_valid_initial = valid_report arr in
  if is_valid_initial then true
  else
    let len = Array.length arr in
    let rec aux skip =
      if skip = len then false
      else
        let new_arr = arr_exclude arr skip in
        let is_valid = valid_report new_arr in
        if is_valid then true
        else aux (skip+1)
    in
    aux 0

let inverser arg = not arg
let%test _ = inverser true = false
let%test _ = inverser true = true

let read_file file = In_channel.with_open_bin file In_channel.input_lines