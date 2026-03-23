open Bench_lib
open Alcotest.V1
open QCheck2

(** we measure string lengths as inputs, so test values from that range. *)
let input = Gen.(0 -- Sys.max_string_length)

let distribution =
  let open Gen in
  let open Distribution in
  let+ values = Gen.list input in
  (* limit total sum to Sys.max_string_length *)
  values
  |> List.fold_left
       (fun (sum, acc) e ->
          let sum' = sum + e in
          if sum' < 0 then sum, 0 :: acc else sum', e :: acc)
       (0, [])
  |> snd
  |> List.to_seq
  |> of_seq


let print = Print.(contramap (Fmt.to_to_string Distribution.dump) string)

(* these test can be somewhat slower due to Gen.list, so use a smaller long_factor *)
let long_factor = 20

let of_summary_to_summary =
  let open Distribution in
  Test.make ~long_factor ~name:__FUNCTION__ distribution ~print
  @@ fun dist ->
  let view = to_view dist in
  let t' = of_view view in
  let r = t' = dist in
  if not r
  then
    Test.fail_reportf
      "@[<v>dist: %a@,view: %a@,of_view: %a@]"
      dump
      dist
      Fmt.(Dump.array @@ pair int int)
      view
      dump
      t';
  r


let generated_stats =
  let open Distribution in
  Test.make ~long_factor ~name:__FUNCTION__ distribution ~print
  @@ fun dist ->
  let generated = generate dist |> Array.of_seq in
  let t' = of_seq (Array.to_seq generated) in
  let r = t' = dist in
  if not r
  then
    Test.fail_reportf
      "@[<v>dist:  %a@,generated: %a@,dist': %a@]"
      dump
      dist
      Fmt.(Dump.array int)
      generated
      dump
      t';
  r


let () =
  [ ( "qcheck"
    , [ of_summary_to_summary; generated_stats ]
      |> List.map QCheck_alcotest.(to_alcotest ~long:true ~verbose:true) )
  ]
  |> run __FILE__
