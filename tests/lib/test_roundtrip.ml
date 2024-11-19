let pp_rpc = Fmt.of_to_string Rpc.to_string

let make_test (type a) name kind input to_rpc to_wire pp_wire of_wire of_rpc =
  let open Alcotest.V1 in
  test_case name `Quick
  @@ fun () ->
  let module T = (val kind : TESTABLE with type t = a) in
  Format.printf "%s: %a@." name T.pp input;
  (* log line by line: any of these can raise an exception,
     and we need to see how far we've got to debug *)
  let rpc_in = input |> to_rpc in
  Format.printf " -> %s@." (Rpc.to_string rpc_in);
  let wire = rpc_in |> to_wire in
  Format.printf " -> %a@." pp_wire wire;
  let rpc_out = wire |> of_wire in
  Format.printf " -> %s@." (Rpc.to_string rpc_out);
  let actual = rpc_out |> of_rpc in
  Format.printf " -> %a@." T.pp actual;
  let msg =
    Format.asprintf
      "%s: %a -> %s -> %a -> %s -> %a"
      name
      T.pp
      input
      (Rpc.to_string rpc_in)
      pp_wire
      wire
      (Rpc.to_string rpc_out)
      T.pp
      actual
  in
  check' kind ~msg ~expected:input ~actual


let rpc_of_base64_encode str = str |> Base64.encode_string |> Rpc.rpc_of_base64

let make_tests name to_wire pp_wire of_wire =
  let make_test name' kind input to_rpc of_rpc =
    make_test (name ^ "/" ^ name') kind input to_rpc to_wire pp_wire of_wire of_rpc
  in
  let open Alcotest.V1 in
  [ make_test "Int" int Int.max_int Rpc.rpc_of_int Rpc.int_of_rpc
  ; make_test "Int32" int32 Int32.max_int Rpc.rpc_of_int32 Rpc.int32_of_rpc
  ; make_test "Bool" bool true Rpc.rpc_of_bool Rpc.bool_of_rpc
  ; make_test "Float" (float 0.1) 2.3 Rpc.rpc_of_float Rpc.float_of_rpc
  ; make_test "String" string "foo" Rpc.rpc_of_string Rpc.string_of_rpc
  ; make_test
      "enum"
      (list string)
      [ "a"; "x" ]
      (fun l -> Rpc.Enum (l |> List.map Rpc.rpc_of_string))
      (function
        | Rpc.Enum l -> List.map Rpc.string_of_rpc l
        | _ -> failwith "bad value")
  ; make_test
      "dict"
      (list (pair string int))
      [ "a", 1; "b", 2 ]
      (fun l -> Rpc.Dict (l |> List.map (fun (k, v) -> k, Rpc.rpc_of_int v)))
      (function
        | Rpc.Dict l -> List.map (fun (k, v) -> k, Rpc.int_of_rpc v) l
        | _ -> failwith "bad value")
  ; make_test "unit" unit () Rpc.rpc_of_unit Rpc.unit_of_rpc
(* JSONRPC is broken here, will be reenabled in followup commits
  ; make_test "DateTime" string "2024-01-01" Rpc.rpc_of_dateTime Rpc.dateTime_of_rpc
  ; make_test "Int32.compat" int32 Int32.min_int (fun i -> Rpc.Int32 i) Rpc.int32_of_rpc
  ; make_test "Base64" string "\x01\x00\x02" rpc_of_base64_encode Rpc.base64_of_rpc*)
  ]


let tests : unit Alcotest.V1.test_case list =
  [ make_tests
      "XMLRPC"
      (fun rpc -> Xmlrpc.to_string rpc)
      Fmt.string
      (fun str -> Xmlrpc.of_string str)
  ; make_tests
      "JSONRPC"
      (fun rpc -> Jsonrpc.to_string rpc)
      Fmt.string
      (fun str -> Jsonrpc.of_string str)
  ; make_tests "Rpc.t" Fun.id pp_rpc Fun.id
  ]
  |> List.concat
