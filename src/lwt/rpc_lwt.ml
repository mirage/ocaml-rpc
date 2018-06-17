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

module LwtIdl = Idl.Make(LwtM)
module M = struct
  include LwtIdl.ImplM
  (* TODO: can we avoid Obj.magic here? *)
  let lwt x : 'a Lwt.t = Obj.magic (LwtM.unbox x)
end
include LwtIdl