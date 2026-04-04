let tests =
  let f _ = Rpc.Null in
  let input = List.init 164 (fun _ -> Rpc.String "") in
  [ Bechamel.Test.make_grouped
      ~name:"assoc_all"
      [ Bechamel.Test.make_indexed
          ~name:"List.assoc"
          ~args:[ 1; 2; 4; 8; 16; 32; 64 ]
          (fun n ->
             let l = List.init n (fun i -> string_of_int i, i) in
             let keys = List.map fst l in
             Bechamel.Staged.stage (fun () ->
               keys |> List.map @@ fun key -> List.assoc key l))
      ; Bechamel.Test.make_indexed
          ~name:"Map.find"
          ~args:[ 1; 2; 4; 8; 16; 32; 64 ]
          (fun n ->
             let module StringMap = Map.Make (String) in
             let m = Seq.init n (fun i -> string_of_int i, i) |> StringMap.of_seq in
             let keys = StringMap.bindings m |> List.map fst in
             Bechamel.Staged.stage (fun () ->
               keys |> List.map @@ fun key -> StringMap.find key m))
      ; Bechamel.Test.make_indexed
          ~name:"Map.init+find"
          ~args:[ 1; 2; 4; 8; 16; 32; 64 ]
          (fun n ->
             let module StringMap = Map.Make (String) in
             let l = List.init n (fun i -> string_of_int i, i) in
             let keys = List.map fst l in
             Bechamel.Staged.stage (fun () ->
               let m = l |> List.to_seq |> StringMap.of_seq in
               keys |> List.map @@ fun key -> StringMap.find key m))
      ]
  ; Bechamel.Test.make_grouped
      ~name:"map"
      [ Bechamel.Test.make
          ~name:"List.map"
          (Bechamel.Staged.stage (fun () -> List.map f input))
      ; Bechamel.Test.make
          ~name:"List.rev_map + List.rev"
          (Bechamel.Staged.stage (fun () -> List.rev_map f input |> List.rev))
      ]
  ]


let () = Bechamel_simple_cli.cli tests
