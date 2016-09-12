open Idl
open Rpc
open Example3_idl

module PClient=Datapath(GenClient)
module PCmds=Datapath(Cmdlinergen)
module DClient=Data(GenClient)
module DCmds=Data(Cmdlinergen)

let default_cmd =
  let doc = "a cli for an API" in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Cmdliner.Term.info "cli" ~version:"1.6.1" ~doc

(* Use a binary 16-byte length to frame RPC messages *)
let binary_rpc path (call: Rpc.call) : Rpc.response Rpc.Monad.error_or =
  let sockaddr = Unix.ADDR_UNIX path in
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect s sockaddr;
  let ic = Unix.in_channel_of_descr s in
  let oc = Unix.out_channel_of_descr s in
  let msg_buf = Jsonrpc.string_of_call call in
  let len = Printf.sprintf "%016d" (String.length msg_buf) in
  output_string oc len;
  output_string oc msg_buf;
  flush oc;
  let len_buf = String.make 16 '\000' in
  really_input ic len_buf 0 16;
  let len = int_of_string len_buf in
  let msg_buf = String.make len '\000' in
  really_input ic msg_buf 0 len;
  let (response: Rpc.response) = Jsonrpc.response_of_string msg_buf in
  Rpc.Monad.return response

(*let server_cmd =
  let doc = "Start the server" in
  Cmdliner.Term.(const Example2_server.start_server $ const ()),
  Cmdliner.Term.info "server" ~doc*)

let cli () =
  let open Rpc.Monad in
  let rpc = binary_rpc "path" in
  Cmdliner.Term.eval_choice default_cmd [(*server_cmd;*)
    PCmds.open_ rpc;
    PCmds.attach rpc;
    PCmds.activate rpc;
    PCmds.deactivate rpc;
    PCmds.detach rpc;
    PCmds.close rpc;
    DCmds.copy rpc;
    DCmds.mirror rpc;
    DCmds.stat rpc;
    DCmds.cancel rpc;
    DCmds.destroy rpc;
    DCmds.ls rpc;
  ]

let _ = cli ()
