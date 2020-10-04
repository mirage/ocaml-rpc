type return_record =
  { result : string
  ; metadata : (int * int) list
  ; extras : string option
  }
[@@deriving rpcty]

type variant_t =
  | Foo of string list
  | Bar
  | Baz of float
[@@deriving rpcty]

module IDL = Idl.Make (Idl.IdM)

module type ABSMOD = sig
  type t

  val of_string : string -> t
  val string_of : t -> string
  val init : t
end

module AbstractMod : ABSMOD = struct
  type t = string

  let of_string t = t
  let string_of t = t
  let init = "hello"
end

module API (R : Idl.RPC) = struct
  open R
  open Idl

  let description =
    let open Idl.Interface in
    { name = "Test server"
    ; namespace = None
    ; description = [ "Test interface" ]
    ; version = 1, 0, 0
    }


  let abstr =
    let open Rpc.Types in
    { name = "abstr"
    ; ty =
        Abstract
          { aname = "abstr"
          ; test_data = [ AbstractMod.init ]
          ; rpc_of = (fun t -> Rpc.String (AbstractMod.string_of t))
          ; of_rpc =
              (function
              | Rpc.String s -> Ok (AbstractMod.of_string s)
              | _ -> Error (`Msg "bad"))
          }
    ; description = [ "Abstract" ]
    }


  let implementation = implement description

  (* Construct a bunch of arguments to use in our RPCs *)
  let arg1 = Param.mk ~name:"arg1" Rpc.Types.string
  let argx = Param.mk ~name:"x" Rpc.Types.int

  let argopt =
    Param.mk
      ~name:"opt"
      Rpc.Types.{ name = "string opt"; description = []; ty = Option (Basic String) }


  let argv = Param.mk ~name:"v" variant_t
  let argi = Param.mk ~name:"i" Rpc.Types.int64
  let argu = Param.mk ~name:"return_u" Rpc.Types.unit
  let return = Param.mk ~name:"return" return_record
  let abs = Param.mk ~name:"abs" abstr
  let argu_noname = Param.mk Rpc.Types.unit

  (* We'll use the default error type *)
  let e = Idl.DefaultError.err

  (* Construct 3 RPC definitions *)
  let rpc1 = declare "rpc1" [ "Test RPC 1" ] (arg1 @-> argx @-> returning return e)
  let rpc2 = declare "rpc2" [ "Test RPC 2" ] (argopt @-> argv @-> returning argu e)
  let rpc3 = declare "rpc3" [ "Test RPC 3" ] (argi @-> returning argi e)
  let rpc4 = declare "rpc4" [ "Test RPC 4" ] (abs @-> returning arg1 e)
  let rpc5 = declare "rpc5" [ "Test RPC 5" ] (argopt @-> argu_noname @-> returning arg1 e)
end

module ImplM = struct
  open IDL.ErrM

  let rpc1 arg1 x =
    if x = 5
    then return_err (Idl.DefaultError.InternalError "Boo")
    else (
      Printf.printf "rpc1: %s %d\n" arg1 x;
      return { result = "OK!"; metadata = [ 1, 2; 3, 4 ]; extras = Some "bar" })


  let rpc2 opt v =
    (match opt with
    | Some s -> Printf.printf "Got an optional string: %s\n" s
    | None -> ());
    (match v with
    | Foo ss -> Printf.printf "Foo: [%s]\n" (String.concat ";" ss)
    | Bar -> Printf.printf "Bar\n"
    | Baz f -> Printf.printf "Baz: %f\n" f);
    return ()


  let rpc3 i =
    Printf.printf "%Ld\n" i;
    return (Int64.add i 1L)


  let rpc4 abs = return (Printf.sprintf "Abs: %s\n" (AbstractMod.string_of abs))
  let rpc5 _str_opt () = return "good"
end

let rpc rpc_fn call =
  let call_string = Jsonrpc.string_of_call call in
  Printf.printf "rpc function: call_string='%s'\n" call_string;
  let call = Jsonrpc.call_of_string call_string in
  let response = rpc_fn call |> Idl.IdM.run in
  let response_str = Jsonrpc.string_of_response response in
  Printf.printf "rpc function: response_string = '%s'\n" response_str;
  Idl.IdM.return (Jsonrpc.response_of_string response_str)


module Server = API (IDL.GenServer ())
module Client = API (IDL.GenClient ())

let main () =
  Server.rpc1 ImplM.rpc1;
  Server.rpc2 ImplM.rpc2;
  Server.rpc3 ImplM.rpc3;
  Server.rpc4 ImplM.rpc4;
  Server.rpc5 ImplM.rpc5;
  let funcs = Server.implementation in
  let rpc r = rpc (IDL.server funcs) r in
  let body =
    let open IDL.ErrM in
    Client.rpc1 rpc "test argument" 2
    >>= fun result ->
    Printf.printf
      "result.result='%s', metadata=[%s]\n"
      result.result
      (String.concat
         ";"
         (List.map (fun (a, b) -> Printf.sprintf "(%d,%d)" a b) result.metadata))
    |> return
    >>= fun () ->
    checked_bind
      (Client.rpc1 rpc "test argument" 5)
      (fun result ->
        Printf.printf
          "result.result='%s', metadata=[%s]\n"
          result.result
          (String.concat
             ";"
             (List.map (fun (a, b) -> Printf.sprintf "(%d,%d)" a b) result.metadata))
        |> return)
      (fun err ->
        Printf.printf
          "Error: %s\n"
          (match err with
          | Idl.DefaultError.InternalError s -> s)
        |> return)
    >>= fun () ->
    Client.rpc2 rpc None (Foo [ "hello"; "there" ])
    >>= fun _ ->
    Client.rpc2 rpc (Some "Optional") (Foo [ "hello"; "there" ])
    >>= fun _ ->
    Client.rpc3 rpc 999999999999999999L
    >>= fun i ->
    Client.rpc4 rpc AbstractMod.init
    >>= fun s ->
    return (Printf.printf "%Ld,%s\n" i s)
    >>= fun () -> Client.rpc5 rpc None () >>= fun s -> return (Printf.printf "%s" s)
  in
  IDL.T.get body


let run () =
  match Idl.IdM.run (main ()) with
  | Ok () -> ()
  | Error _ -> failwith "Failed"


let tests = [ Alcotest.test_case "basic Lwt client-server test" `Quick run ]
