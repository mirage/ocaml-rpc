type return_record = { result : string; metadata : (int * int) list; }

type variant = Foo of string list | Bar | Baz of float

type failure = string * (string * string) list

exception RpcFailure of (string * (string * string) list)

module Client : sig
  val rpc1 : (Rpc.call -> Rpc.response) -> arg1:string -> int -> return_record
  val rpc2 : (Rpc.call -> Rpc.response) -> ?opt:string -> variant -> unit
  val rpc3 : (Rpc.call -> Rpc.response) -> int64 -> int64
  module SubModule : sig
    val rpc4 : (Rpc.call -> Rpc.response) -> int64 -> int64
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
