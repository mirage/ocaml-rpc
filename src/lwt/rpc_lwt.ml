module LwtM: Idl.RPCMONAD = struct
  type 'a m = 'a Lwt.t
  type 'a box = { box: 'a m }
  type ('a, 'b) t = ('a, 'b) Result.result box

  type rpcfn = Rpc.call -> Rpc.response m

  let box x = { box=x }
  let unbox { box } = box
  let bind x f = Lwt.bind x f
  let return x = Lwt.return x
  let fail exn = Lwt.fail exn
end

module GenClient = Idl.MakeGenClient(LwtM)
module ServerImpl = Idl.MakeServerImpl(LwtM)
module GenServer = Idl.MakeGenServer(LwtM)