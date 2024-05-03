module Index = ExponentialHistogramBucketIndex

module Bucket = struct
  (* we use the sum, and not the mean to avoid floating point precision issues
     on large integers above 2**53 *)
  type t =
    { mutable sum : int
    ; mutable count : int
    ; groups : int (** top-level Rpc objects *)
    }

  let v ~sum ~count = { sum; count; groups = 1 }
  let zero _ = v ~sum:0 ~count:0
  let round_div a b = if b = 0 then 0 else (a + (b / 2)) / b
  let sum t = round_div t.sum t.groups
  let count t = round_div t.count t.groups
  let has_count t = t.count > 0

  let update t v =
    (* We don't need to worry about integer overflow when calculating the sum,
       because it must be possible to serialize the entire RPC.t value into a single string,
       which is limited by [Sys.max_string_length]
    *)
    let sum = t.sum + v in
    if sum < 0 then Fmt.invalid_arg "Sum overflow: %d + %d" t.sum v;
    t.sum <- sum;
    t.count <- t.count + 1


  (* avoid t.sum/t.groups becoming smaller than the exp bucket:
     only count groups that had a value in the bucket
  *)
  let groups t = if t.count > 0 then t.groups else 0

  let merge a b =
    { sum = a.sum + b.sum; count = a.count + b.count; groups = groups a + groups b }


  let generate t =
    if count t = 0
    then Seq.empty
    else (
      let index = Index.of_int (sum t / count t) in
      let lo_excl, hi_incl = Index.range index in
      let lo_incl = lo_excl + 1 in
      t
      |> Seq.unfold
         @@ fun t ->
         let sum' = sum t
         and count' = count t in
         if count' = 0
         then None
         else (
           let mean = sum' / count' in
           (* if all following values are at the high end of the range, can we still reach sum? *)
           let max_delta = Int.max 0 (-(sum' - mean - (hi_incl * (count' - 1)))) in
           let delta = count' / 2 in
           let delta' = Int.min max_delta delta in
           let v = mean - delta' in
           let v = v |> Int.max lo_incl |> Int.min hi_incl in
           let t = { sum = t.sum - v; count = t.count - 1; groups = 1 } in
           Some (v, t)))


  let dump = Fmt.(Dump.(record [ field "sum" sum int; field "count" count int ]))
end

type t = Bucket.t array

let len = 1 + Index.(of_int Sys.max_string_length |> to_index)
let make () = Array.init len Bucket.zero

let update t value =
  let index = Index.of_int value in
  Bucket.update t.(Index.to_index index) value


let merge a b = Array.map2 Bucket.merge a b
let to_seq t = t |> Array.to_seq |> Seq.filter Bucket.has_count

let of_seq seq =
  let t = make () in
  let () =
    seq
    |> Seq.iter
       @@ fun bucket ->
       (* determine which index the bucket should be placed at *)
       let mean = Bucket.sum bucket / Bucket.count bucket in
       let index = Index.(mean |> of_int |> to_index) in
       if Bucket.has_count t.(index)
       then Fmt.invalid_arg "Bucket already used at %d: %a" index Bucket.dump t.(index);
       t.(index) <- bucket
  in
  t


let dump = Fmt.(Dump.array Bucket.dump)
