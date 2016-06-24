(* All the ppx tests *)
open OUnit

let check_marshal_unmarshal : 'a * Rpc.t * ('a -> Rpc.t) * (Rpc.t -> ('a,_) Result.result) -> unit = fun (x, r, marshal, unmarshal) ->
  let r' = marshal x in
  let x' = unmarshal r in
  assert_equal (Result.Ok x) x';
  assert_equal r r';

type test_int = int [@@deriving rpc]
let test_int () =
  check_marshal_unmarshal (1, Rpc.Int 1L, rpc_of_test_int, test_int_of_rpc)

type test_int32 = int32 [@@deriving rpc]
let test_int32 () =
  check_marshal_unmarshal (1l, Rpc.Int 1L, rpc_of_test_int32, test_int32_of_rpc)

type test_int64 = int64 [@@deriving rpc]
let test_int64 () =
  check_marshal_unmarshal (1L, Rpc.Int 1L, rpc_of_test_int64, test_int64_of_rpc)

type test_unit = unit [@@deriving rpc]
let test_unit () =
  check_marshal_unmarshal ((), Rpc.Null,  rpc_of_test_unit, test_unit_of_rpc)

type test_string = string [@@deriving rpc]
let test_string () =
  check_marshal_unmarshal ("test string", Rpc.String "test string", rpc_of_test_string, test_string_of_rpc)

type test_float = float [@@deriving rpc]
let test_float () =
  check_marshal_unmarshal (2.0, Rpc.Float 2.0, rpc_of_test_float, test_float_of_rpc)

type test_bool = bool [@@deriving rpc]
let test_bool () =
  check_marshal_unmarshal (true, Rpc.Bool true, rpc_of_test_bool, test_bool_of_rpc)

type test_char = char [@@deriving rpc]
let test_char () =
  check_marshal_unmarshal ('x', Rpc.Int (Char.code 'x' |> Int64.of_int), rpc_of_test_char, test_char_of_rpc)

let suite =
  "basic_tests" >:::
  [
    "int" >:: test_int;
    "int32" >:: test_int32;
    "int64" >:: test_int64;
    "unit" >:: test_unit;
    "string" >:: test_string;
    "float" >:: test_float;
    "bool" >:: test_bool;
    "char" >:: test_char; 
  ]

let _ =
  run_test_tt_main suite


