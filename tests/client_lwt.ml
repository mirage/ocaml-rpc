module Monad = struct
  type 'a t = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return
  let fail = Lwt.fail
end

let (>>=) = Lwt.bind

module ImplM = struct
  include Monad

  type context = unit

  let handle_failure = Lwt.catch

  let rpc1 context ~arg1 x =
    (if x=5 then failwith "Boo");
    Printf.printf "rpc1: %s %d\n" arg1 x;
    return {
      Idl_test.Old.result = "OK!";
      Idl_test.Old.metadata = [(1,2);(3,4)];
      Idl_test.Old.extras = Some "bar";
    }

  let rpc2 context ?opt v =
    (match opt with
    | Some s -> Printf.printf "Got an optional string: %s" s;
    | None -> ());
    (match v with
    | Idl_test.Foo ss ->
      Printf.printf "Foo: [%s]\n" (String.concat ";" ss)
    | Idl_test.Bar ->
      Printf.printf "Bar\n"
    | Idl_test.Baz f ->
      Printf.printf "Baz: %f\n" f);
    return ()

  let rpc3 context i =
    Printf.printf "%Ld\n" i;
    Lwt.return (Int64.add i 1L)

  module SubModule = struct
    let rpc4 context i =
      Printf.printf "%Ld\n" i;
      Lwt.return (Int64.sub i 1L)
  end
end

module MyServer=Idl_test.ServerM(ImplM)

module RPCM = struct
  include Monad

  let rpc call =
    let call_string = Jsonrpc.string_of_call call in
    Printf.printf "rpc function: call_string='%s'\n" call_string;
    let call = Jsonrpc.call_of_string call_string in
    MyServer.process () call >>= fun response -> 
    let response_str = Jsonrpc.string_of_response response in
    Printf.printf "rpc function: response_string = '%s'\n" response_str;
    Lwt.return (Jsonrpc.response_of_string response_str)
end

module Client = Idl_test.ClientM(RPCM)



let main () =
  Client.rpc1 ~arg1:"test argument" 2 >>= fun result -> 
  Printf.printf "result.result='%s', metadata=[%s]\n"
    result.Idl_test.Old.result (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) result.Idl_test.Old.metadata));

  Lwt.catch (fun () ->  
      Client.rpc1 ~arg1:"test argument" 5 >>= fun result -> 
      Printf.printf "result.result='%s', metadata=[%s]\n"
        result.Idl_test.Old.result (String.concat ";" (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) result.Idl_test.Old.metadata));
      Lwt.return ()) 
    (fun e -> Printf.printf "Got a failure: %s\n" (Printexc.to_string e);
        Lwt.return ()) >>= fun _ -> 

  Client.rpc2 (Idl_test.Foo ["hello";"there"]) >>= fun _ -> 
  Client.rpc2 ~opt:"Optional" (Idl_test.Foo ["hello";"there"]) >>= fun _ -> 
  Client.rpc3 999999999999999999L >>= fun i -> 
  Printf.printf "%Ld\n" i;
  Client.SubModule.rpc4 3L

let _ = Lwt_main.run (main ())
