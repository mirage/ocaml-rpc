(* based on bechamel example code *)
open Bechamel
open Toolkit

let instances = Instance.[ monotonic_clock; minor_allocated; major_allocated ]

let benchmark cfg tests =
  let n = List.length tests in
  tests
  |> List.to_seq
  |> Seq.mapi (fun i test ->
    let name = Test.Elt.name test in
    Format.eprintf "Running benchmark %u/%u %s ...@?" (i + 1) n name;
    let results = Benchmark.run cfg instances test in
    Format.eprintf "@.";
    name, results)
  |> Hashtbl.of_seq


let analyze raw_results =
  let ols ~bootstrap =
    Analyze.ols ~bootstrap ~r_square:true ~predictors:[| Measure.run |]
  in
  let results =
    List.map
      (fun instance ->
         let f bootstrap = Analyze.all (ols ~bootstrap) instance raw_results in
         try f 3000 with
         | _ -> f 0)
      instances
  in
  Analyze.merge (ols ~bootstrap:3000) instances results, raw_results


let () = List.iter (fun i -> Bechamel_notty.Unit.add i (Measure.unit i)) instances

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window ~predictor:Measure.run results


open Notty_unix

let cli cfg tests store =
  let results, raw_results = tests |> benchmark cfg |> analyze in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  img (window, results) |> eol |> output_image;
  results
  |> Hashtbl.iter
     @@ fun label results ->
     if label = Measure.label Instance.monotonic_clock
     then (
       let units = Bechamel_notty.Unit.unit_of_label label in
       results
       |> Hashtbl.iter
          @@ fun name ols -> Format.printf "%s (%s):@, %a@." name units Analyze.OLS.pp ols);
     store
     |> Option.iter
        @@ fun dir ->
        raw_results
        |> Hashtbl.iter
           @@ fun label results ->
           let label =
             String.map
               (function
                 | '/' -> '_'
                 | c -> c)
               label
           in
           let file = Filename.concat dir (label ^ ".dat") in
           Out_channel.with_open_text file
           @@ fun out ->
           let label = Measure.label Instance.monotonic_clock in
           results.Benchmark.lr
           |> Array.iter
              @@ fun measurement ->
              let repeat = Measurement_raw.run measurement in
              let avg = Measurement_raw.get ~label measurement /. repeat in
              (* ministat wants to compare individual measurements, but all we have is a sum. *)
              Printf.fprintf out "%.16g\n" avg


open Cmdliner

let cli tests =
  let tests = List.concat_map Test.elements tests in
  let cmd =
    let test_names = tests |> List.map (fun t -> Test.Elt.name t, t) in
    let filtered =
      let doc =
        Printf.sprintf
          "Choose the benchmarks to run. $(docv) must be %s"
          Arg.(doc_alts_enum test_names)
      in
      Arg.(
        value
        & pos_all (enum test_names) tests
        & info [] ~absent:"all" ~doc ~docv:"BENCHMARK")
    and cfg =
      let open Term.Syntax in
      let+ quota =
        Arg.(
          value
          & opt (some float) None
          & info [ "quota" ] ~doc:"Maximum time per benchmark" ~docv:"SECONDS")
      and+ start =
        Arg.(
          value
          & opt (some int) None
          & info [ "start" ] ~doc:"Starting iteration count" ~docv:"COUNT")
      in
      Benchmark.cfg ?quota:(Option.map Time.second quota) ?start ()
    and store =
      Arg.(
        value
        & opt (some dir) None
        & info
            [ "output-dir"; "d" ]
            ~doc:
              "directory to save the raw results to. The output can be used by ministat"
            ~docv:"DIRECTORY")
    in
    let info = Cmd.info "benchmark" ~doc:"Run benchmarks" in
    Cmd.v info Term.(const cli $ cfg $ filtered $ store)
  in
  exit (Cmd.eval cmd)
