let run () =
  let t = "<provision><value><int>4</int></value>" in
  let r = Rpc.rpc_of_string t in
  Printf.printf "r = %s\n%!" (Rpc.to_string r);
  let t' = Rpc.string_of_rpc r in
  Printf.printf "t = t : %b'\n%!" (t = t');
  assert (t = t')


let run_not_strict () =
  let test = Rpc.Dict [ "foo", Rpc.String "&"; "bar", Rpc.Int (Int64.of_int 3) ] in
  let str = Xmlrpc.to_string ~strict:false test in
  assert (
    str
    = "<value><struct><member><name>foo</name><value>&amp;</value></member><member><name>bar</name><value>3</value></member></struct></value>"
  )


let run_strict () =
  let test = Rpc.Dict [ "foo", Rpc.String "&"; "bar", Rpc.Int (Int64.of_int 3) ] in
  let str = Xmlrpc.to_string ~strict:true test in
  assert (
    str
    = "<value><struct><member><name>foo</name><value>&amp;</value></member><member><name>bar</name><value><i8>3</i8></value></member></struct></value>"
  )


let tests =
  [ "test", `Quick, run
  ; "test not strict", `Quick, run_not_strict
  ; "test strict", `Quick, run_strict
  ]
