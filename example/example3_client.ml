open Idl
open Rpc

module PClient=Datapath(GenClient)
module PCmds=Datapath(Cmdlinergen)
module DClient=Data(GenClient)
module DCmds=Data(Cmdlinergen)

let default_cmd =
  let doc = "a cli for an API" in
  Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Cmdliner.Term.info "cli" ~version:"1.6.1" ~doc

(*let server_cmd =
  let doc = "Start the server" in
  Cmdliner.Term.(const Example2_server.start_server $ const ()),
  Cmdliner.Term.info "server" ~doc*)

let cli () =
  let open Rpc.Monad in
  Cmdliner.Term.eval_choice default_cmd [(*server_cmd;*)
    PCmds.open_;
    PCmds.attach;
    PCmds.activate;
    PCmds.deactivate;
    PCmds.detach;
    PCmds.close;
    DCmds.copy;
    DCmds.mirror;
    DCmds.stat;
    DCmds.cancel;
    DCmds.destroy;
    DCmds.ls;      
  ]

let _ = cli ()
