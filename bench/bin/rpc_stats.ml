open Bench_lib

let analyze rpc = Rpc_stats.(rpc |> seq_of_rpc empty |> stats_of_seq)

let () =
  Sys.argv
  |> Array.to_seq
  |> Seq.drop 1
  |> Seq.fold_left
       (fun stats path ->
          In_channel.with_open_bin path
          @@ fun input ->
          let response = Xmlrpc.response_of_in_channel input in
          let stats' = analyze response.contents in
          Rpc_stats.merge stats stats')
       Rpc_stats.(stats_of_seq empty)
  |> Format.printf "%a@." Rpc_stats.pp_stats
