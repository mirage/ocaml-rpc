open Bench_lib
module Index = ExponentialHistogramBucketIndex
open QCheck2

(** Unique corner cases around {!val:Index.range} endpoints.
    Testing these values ensures there are no gaps in the coverage of {!val:Index.of_int}. *)
let corner_cases =
  Index.all
  |> Seq.concat_map (fun t ->
    let lo, hi = Index.range t in
    List.to_seq [ lo; lo + 1; hi - 1; hi; hi + 1 ])
  |> Seq.filter (fun x -> 0 <= x && x <= Sys.max_string_length)
  |> List.of_seq
  |> List.sort_uniq Int.compare


(** we measure string lengths as inputs, so test values from that range. *)
let input = Gen.(graft_corners (0 -- Sys.max_string_length) corner_cases ())

let index = Gen.oneofl (List.of_seq Index.all)

(* these tests are all very quick, use a larger long_factor *)
let long_factor = 100

let test_update =
  let open Histogram in
  Test.make ~long_factor ~name:__FUNCTION__ input ~print:Print.int
  @@ fun v ->
  let hist = make () in
  update hist v;
  match hist |> to_seq |> List.of_seq with
  | [ one ] ->
    let sum = Bucket.sum one
    and count = Bucket.count one in
    if sum <> v || count <> 1
    then Test.fail_reportf "Bucket mismatch for %d: %a" v Bucket.dump one;
    true
  | l -> Test.fail_reportf "Only one bucket expected: %a" (Fmt.Dump.list Bucket.dump) l


let histogram_input =
  (* / 100 to avoid overflow, small_list generates max 100 elements *)
  Gen.(small_list (0 -- (Sys.max_floatarray_length / 100)))


let to_view h =
  h
  |> Histogram.to_seq
  |> Seq.map (fun b -> Histogram.Bucket.sum b, Histogram.Bucket.count b)
  |> List.of_seq


let print_hist = Print.(contramap to_view @@ list @@ pair int int)

let to_histogram l =
  let hist = Histogram.make () in
  l |> List.iter (Histogram.update hist);
  hist


let test_merge =
  let gen = Gen.(pair histogram_input histogram_input)
  and print = Print.(pair (list int) (list int)) in
  Test.make ~long_factor ~name:__FUNCTION__ gen ~print
  @@ fun (x, y) ->
  let a = to_histogram x
  and b = to_histogram y
  and expected = to_histogram (List.rev_append x y) in
  let actual = Histogram.merge a b in
  let ok =
    Seq.for_all2
      (fun actual expected ->
         let open Histogram in
         let asum, acount, agroups =
           Bucket.sum actual, Bucket.count actual, Bucket.groups actual
         and esum, ecount = Bucket.sum expected, Bucket.count expected in
         (agroups = 1 || agroups = 2)
         && (esum + (agroups / 2)) / agroups = asum
         && (ecount + (agroups / 2)) / agroups = acount)
      (Histogram.to_seq actual)
      (Histogram.to_seq expected)
  in
  if not ok
  then
    Test.fail_reportf
      "Merge mismatch: merge %a %a@ =@ %a@ <>@ %a/2"
      Fmt.(Dump.list int)
      x
      Fmt.(Dump.list int)
      y
      Histogram.dump
      actual
      Histogram.dump
      expected;
  true


let test_bucket_v =
  Test.make
    ~long_factor
    ~name:__FUNCTION__
    Gen.(pair input input)
    ~print:Print.(pair int int)
  @@ fun (sum, count) ->
  let open Histogram in
  let b = Bucket.v ~sum ~count in
  Bucket.sum b = sum && Bucket.count b = count && Bucket.groups b = 1


let bucket =
  let open Gen in
  (* prefer smaller value, so that we get more duplicates/values in a single bucket *)
  let+ sum, count = pair Gen.(oneof [ 0 -- 256; input ]) small_int in
  Histogram.Bucket.v ~sum ~count


let test_bucket_generate =
  let open Histogram in
  let print =
    Print.(
      contramap
        (fun b -> Bucket.sum b, Bucket.count b, Bucket.groups b)
        (tup3 int int int))
  in
  Test.make ~long_factor ~name:__FUNCTION__ bucket ~print
  @@ fun bucket ->
  let generated = Bucket.generate bucket |> List.of_seq in
  if Bucket.count bucket = 0
  then List.length generated = 0
  else (
    let mean = Bucket.sum bucket / Bucket.count bucket in
    let index = Index.of_int mean in
    let lo_excl, hi_incl = Index.range index in
    (* must be possible to reach sum *)
    assume (hi_incl * Bucket.count bucket >= Bucket.sum bucket);
    let range_ok = List.for_all (fun v -> lo_excl < v && v <= hi_incl) generated in
    if not range_ok
    then
      Test.fail_reportf
        "Generated values out of range (%d, %d]: %a"
        lo_excl
        hi_incl
        Fmt.(Dump.list int)
        generated;
    let asum = List.fold_left ( + ) 0 generated
    and acount = List.length generated in
    if asum <> Bucket.sum bucket || acount <> Bucket.count bucket
    then
      Test.fail_reportf
        "Generated sum/count doesn't match: %d, %d, expected %a: %a"
        asum
        acount
        Bucket.dump
        bucket
        Fmt.(Dump.list int)
        generated;
    if
      mean <> hi_incl
      && mean <> lo_excl + 1
      && Bucket.count bucket > 1
      && Bucket.sum bucket > 0
    then (
      let unique = List.sort_uniq Int.compare generated |> List.length in
      if unique = 1
      then
        Test.fail_reportf
          "Should generate values other than the mean: %a"
          Fmt.(Dump.list int)
          generated);
    true)


let test_of_seq_to_seq =
  let open Histogram in
  let print = Print.(list int) in
  Test.make ~long_factor ~name:__FUNCTION__ histogram_input ~print
  @@ fun values ->
  let t = to_histogram values in
  let seq = to_seq t in
  let () =
    seq
    |> Seq.iter
       @@ fun b -> if Bucket.count b = 0 then Test.fail_reportf "Got bucket with 0 count"
  in
  let actual = of_seq seq in
  let r = t = actual in
  if not r
  then
    Test.fail_reportf
      "@[<v>t: %a@,-> counts: %a@,-> %a@ mismatch@]"
      dump
      t
      Fmt.(Dump.seq Bucket.dump)
      seq
      dump
      actual;
  r


let () =
  Alcotest.V1.run
    __FILE__
    [ ( "qcheck"
      , [ test_update
        ; test_merge
        ; test_bucket_v
        ; test_bucket_generate
        ; test_of_seq_to_seq
        ]
        |> List.map QCheck_alcotest.(to_alcotest ~long:true ~verbose:true) )
    ]
