module Impl = struct
  type context = unit

  let rpc1 context ~arg1 x =
    (if x=5 then failwith "Boo");
    Printf.printf "rpc1: %s %d\n" arg1 x;
    {
      Idl_test.Old.result = "OK!";
      Idl_test.Old.metadata = [(1,2);(3,4)];
      Idl_test.Old.extras = Some "bar";
    }

  let rpc2 context ?opt v =
    (match opt with
    | Some s -> Printf.printf "Got an optional string: %s" s;
    | None -> ());
    match v with
    | Idl_test.Foo ss ->
      Printf.printf "Foo: [%s]\n" (String.concat ";" ss)
    | Idl_test.Bar ->
      Printf.printf "Bar\n"
    | Idl_test.Baz f ->
      Printf.printf "Baz: %f\n" f

  let rpc3 context i =
    Printf.printf "%Ld\n" i;
    Int64.add i 1L

  module SubModule = struct
    let rpc4 context i =
      Printf.printf "%Ld\n" i;
      Int64.sub i 1L
  end
end

module MyServer=Idl_test.Server(Impl)

module RPC = struct
  let rpc call =
    let call_string = Jsonrpc.string_of_call call in
    Printf.printf "rpc function: call_string='%s'\n" call_string;
    let call = Jsonrpc.call_of_string call_string in
    let response = MyServer.process () call in
    let response_str = Jsonrpc.string_of_response response in
    Printf.printf "rpc function: response_string = '%s'\n" response_str;
    Jsonrpc.response_of_string response_str
end

module Client = Idl_test.Client(RPC)

let _ =
  let result = Client.rpc1 ~arg1:"test argument" 2 in
  Printf.printf "result.result='%s', metadata=[%s]\n"
    result.Idl_test.Old.result (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) result.Idl_test.Old.metadata));

  begin
    try
      let result = Client.rpc1 ~arg1:"test argument" 5 in
      Printf.printf "result.result='%s', metadata=[%s]\n"
        result.Idl_test.Old.result (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) result.Idl_test.Old.metadata));
    with
      e -> Printf.printf "Got a failure: %s\n" (Printexc.to_string e)
  end;
  Client.rpc2 (Idl_test.Foo ["hello";"there"]);
  Client.rpc2 ~opt:"Optional" (Idl_test.Foo ["hello";"there"]);
  let i = Client.rpc3 999999999999999999L in
  Printf.printf "%Ld\n" i;
  Client.SubModule.rpc4 3L;
  
  let test_extend = {
    Idl_test.Old.result = "foo";
    Idl_test.Old.metadata = [(5,6)];
    Idl_test.Old.extras = Some "bar";
  } in
  
  let test_old = Idl_test.Old.rpc_of_return_record test_extend in
  Printf.printf "Checking extension:\n";
  let test_new = Idl_test.return_record_extended_of_rpc test_old in
  Printf.printf "new_field = [%s]\n" (String.concat ";" test_new.Idl_test.new_field);
  Printf.printf "result (should be 'foo') = %s\n" test_new.Idl_test.result;
  Printf.printf "extras (should be 'Some \"bar\"' = %s\n"
    (match test_new.Idl_test.extras with
    | Some x -> Printf.sprintf "Some \"%s\"" x
    | None -> "None")
