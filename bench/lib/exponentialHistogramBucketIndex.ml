type t = int

let rec ilog16_floor p n = if n >= 16 then ilog16_floor (p + 1) (n lsr 4) else p
let ilog16_floor n = ilog16_floor 0 n
let base_pow_index index = 1 lsl (4 * index)

let of_int = function
  | n when n < 0 -> Fmt.invalid_arg "%s: negative value %d" __FUNCTION__ n
  | n when n > Sys.max_string_length ->
    Fmt.invalid_arg "%s: exceeds max_string_length %d" __FUNCTION__ n
  | 0 | 1 -> 0 (* <= zero_threshold *)
  | n ->
    (* could also use Float.frexp, but loses precision beyond 2**52 *)
    let log16 = ilog16_floor n in
    if base_pow_index log16 = n then log16 else log16 + 1


let to_index (t : t) = t

let range = function
  | 0 -> -1, 1
  | idx -> base_pow_index (idx - 1), Int.min Sys.max_string_length (base_pow_index idx)


let all =
  let hi = of_int Sys.max_string_length in
  Seq.init (hi + 1) Fun.id
