type return_record = { result : string; metadata : (int * int) list; }

type variant = Foo of string list | Bar | Baz of float

module type RPC = sig
  val rpc: Rpc.call -> Rpc.response
end

module Client: functor (R: RPC) ->
sig
  val rpc1: arg1:string -> int -> return_record
  val rpc2: ?opt:string -> variant -> unit
  val rpc3: int64 -> int64
  module SubModule : sig
    val rpc4 : int64 -> int64
  end
end

module type Server_impl =
  sig
    type context
    val rpc1 : context -> arg1:string -> int -> return_record
    val rpc2 : context -> ?opt:string -> variant -> unit
    val rpc3 : context -> int64 -> int64
    module SubModule : sig
      val rpc4 : context -> int64 -> int64
    end
  end

module Server : functor (Impl : Server_impl) ->
sig
  val process : Impl.context -> Rpc.call -> Rpc.response
end
