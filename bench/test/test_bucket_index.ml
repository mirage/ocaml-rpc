open Bench_lib
module I = ExponentialHistogramBucketIndex
open QCheck2

(** Unique corner cases around {!val:Index.range} endpoints.
  Testing these values ensures there are no gaps in the coverage of {!val:Index.of_int}.
 *)
let corner_cases =
  I.all
  |> Seq.concat_map (fun t ->
    let lo, hi = I.range t in
    List.to_seq [ lo; lo + 1; hi - 1; hi; hi + 1 ])
  |> Seq.filter (fun x -> 0 <= x && x <= Sys.max_string_length)
  |> List.of_seq
  |> List.sort_uniq Int.compare


(** We measure string lengths as inputs, so test values from that range.
    Also test values around range endpoints as special cases.
 *)
let input = Gen.(graft_corners (0 -- Sys.max_string_length) corner_cases ())

let index = Gen.oneofl (List.of_seq I.all)

(* these tests are all very quick, use a large long_factor *)
let long_factor = 1000

let test_of_int =
  Test.make ~long_factor ~name:__FUNCTION__ input ~print:Print.int
  @@ fun v ->
  let lo, hi = I.range (I.of_int v) in
  let r = lo < v && v <= hi in
  if not r then Test.fail_reportf "%d not in (%d, %d]" v lo hi;
  r


let test_to_index =
  Test.make ~long_factor ~name:__FUNCTION__ index ~print:Print.(contramap I.to_index int)
  @@ fun i ->
  let i = I.to_index i in
  if i < 0 then Test.fail_reportf "Negative index: %d" i;
  true


let test_range =
  Test.make ~long_factor ~name:__FUNCTION__ index ~print:Print.(contramap I.to_index int)
  @@ fun i ->
  let lo_excl, hi_incl = I.range i in
  if lo_excl >= hi_incl
  then Test.fail_reportf "%d, %d is not a valid range" lo_excl hi_incl;
  let check_index v =
    let i' = I.of_int v in
    if i' <> i
    then
      Test.fail_reportf
        "range (%d, %d] does not map back to index %d"
        lo_excl
        hi_incl
        (I.to_index i)
        (I.to_index i')
  in
  check_index (lo_excl + 1);
  check_index hi_incl;
  true


module IntSet = Set.Make (Int)

let all_set = I.all |> Seq.map I.to_index |> IntSet.of_seq

let test_all =
  Test.make ~long_factor ~name:__FUNCTION__ input ~print:Print.int
  @@ fun v ->
  let i = I.of_int v in
  IntSet.mem (I.to_index i) all_set


let () = assert (IntSet.mem 0 all_set)

let () =
  Alcotest.V1.run
    __FILE__
    [ ( "qcheck"
      , [ test_of_int; test_to_index; test_range; test_all ]
        |> List.map QCheck_alcotest.(to_alcotest ~long:true ~verbose:true) )
    ]
