(* decompose
   1. first part looks really easy
   there is an algo or just fn which accepts num fn num and product next num, e.g. next_secret secret -> new_secret

   after we iterate 2000 times and we are here
   for sure part2 will punish for such simplification, so I wait for it :D

   upd. part2
   from every secret we take last digit and it is the price
   we can make a list of changes

   so we can find a best sequence for one secret number, look for the biggest price and show 4 previous changes
   but when 2nd number comes into game, it can be less then than the biggest,
   what we can do it use npointer takes set pointer to biggest number in secret sequence and try starting with biggest overall,
   store result, after move cursor down

   it looks like is the same as brootforce for now, what is the complexity of bruteforce
   2000 / 4 = 500 iterations for every start num
   2500 start numbers * 2000 iterations * 500 is to make a list of variants = 25millions, but we can cache, for sure some of patterns will repeat

   how much is for one comparison iteration
   25 millions of sequences * 500

   how we can optimize it, what about rolling window
   we calc next secret and store window of 4 changes and stores values into cache, lets try
*)

open Core

let mix a b = Int.bit_xor a b
let prune a = a % 16777216

let next_secret s =
  s
  |> (fun s -> s, Int.( * ) s 64)
  |> (fun (s, v) -> Int.bit_xor s v)
  |> (fun s -> Int.( % ) s 16777216)
  |> (fun s -> s, Int.( / ) s 32)
  |> (fun (s, v) -> Int.bit_xor s v)
  |> (fun s -> Int.( % ) s 16777216)
  |> (fun s -> s, Int.( * ) s 2048)
  |> (fun (s, v) -> Int.bit_xor s v)
  |> (fun s -> Int.( % ) s 16777216)
  [@@ocamlformat "disable"]

let%test_unit _ = [%test_result: int] (next_secret 123) ~expect:15887950

let next_secret_n s n =
  let rec aux s n = if n = 0 then s else aux (next_secret s) (n - 1) in
  aux s n
;;

let%test_unit _ = [%test_result: int] (next_secret_n 1 2000) ~expect:8685429

let next_secret_n_arr s n =
  let arr = Array.create ~len:n 0 in
  let rec aux s i =
    if i = n
    then ()
    else (
      arr.(i) <- s;
      aux (next_secret s) (i + 1))
  in
  aux s 0;
  arr
;;

type changes_key = int * int * int * int [@@deriving show, equal, sexp, compare, hash]

module ChangesKey = struct
  type t = changes_key

  let sexp_of_t = sexp_of_changes_key
  let t_of_sexp = changes_key_of_sexp
  let compare = compare_changes_key
  let hash = hash_changes_key
end

let calc_bananas start_secrets =
  let global_cache = Hashtbl.create (module ChangesKey) in
  let calc_changes secret =
    let local_cache = Hashtbl.create (module ChangesKey) in
    let n = 2001 in
    let s = next_secret_n_arr secret n in
    let rec calc_changes_current i =
      if i = n - 4
      then ()
      else (
        let first = if i = 0 then 0 else (s.(i) % 10) - (s.(i - 1) % 10) in
        let price = s.(i + 3) % 10 in
        let key =
          ( first
          , (s.(i + 1) % 10) - (s.(i + 1 - 1) % 10)
          , (s.(i + 2) % 10) - (s.(i + 2 - 1) % 10)
          , (s.(i + 3) % 10) - (s.(i + 3 - 1) % 10) )
        in
        (* printf "key: %s, val: %d\n" (show_changes_key key) last; *)
        if not (Hashtbl.mem local_cache key) then Hashtbl.set local_cache ~key ~data:price;
        calc_changes_current (i + 1))
    in
    calc_changes_current 0;
    Hashtbl.iteri local_cache ~f:(fun ~key ~data ->
      let cur_val = Hashtbl.find_or_add global_cache key ~default:(fun _ -> 0) in
      Hashtbl.set global_cache ~key ~data:(cur_val + data))
  in
  (* printf "bb: %d\n" (Hashtbl.find_exn global_cache (-2, 1, -1, 3)); *)
  List.iter start_secrets ~f:(fun s -> calc_changes s);
  let max = ref Int.min_value in
  Hashtbl.iteri global_cache ~f:(fun ~key:_ ~data:value ->
    if value > !max then (* printf "new max: %s\n" (show_changes_key key); *)
                      max := value);
  !max
;;

let%test_unit _ = [%test_result: int] (calc_bananas [ 1; 2; 3; 2024 ]) ~expect:23

let parse_secrets str =
  str
  |> Str.split (Str.regexp "\n")
  |> List.map ~f:int_of_string
[@@ocamlformat "disable"]

let solve_part1 str =
  let secrets = parse_secrets str in
  secrets
  |> List.map ~f:(fun s -> next_secret_n s 2000)
  |> List.fold ~init:0 ~f:( + )
  [@@ocamlformat "disable"]

let solve_part2 str =
  let secrets = parse_secrets str in
  calc_bananas secrets
;;
