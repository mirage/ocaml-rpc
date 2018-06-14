module Deferred = Async.Deferred

type async_rpcfn = Rpc.call -> Rpc.response Deferred.t

module M : sig
  type 'a async = { async : 'a Deferred.t; }
  type ('a, 'b) t = ('a, 'b) Result.result async
  val return : 'a -> ('a, 'b) Result.result async
  val return_err : 'a -> ('b, 'a) Result.result async
  val checked_bind :
    ('a, 'b) Result.result async ->
    ('a -> 'c async) -> ('b -> 'c async) -> 'c async
  val bind :
    ('a, 'b) Result.result async ->
    ('a -> ('c, 'b) Result.result async) -> ('c, 'b) Result.result async
  val ( >>= ) :
    ('a, 'b) Result.result async ->
    ('a -> ('c, 'b) Result.result async) -> ('c, 'b) Result.result async
  val deferred : 'a async -> 'a Deferred.t
end

(** Client generator similar to {!Idl.GenClient} that uses [Async]. *)
module GenClient () :
  sig
    include Idl.RPC
      with type implementation = unit
       and type 'a res = async_rpcfn -> 'a
       and type ('a,'b) comp = ('a,'b) Result.result M.async
  end

type server_implementation
val server : server_implementation -> async_rpcfn
val combine : server_implementation list -> server_implementation

(** Server generator similar to {!Idl.GenServer} that uses [Async]. *)
module GenServer () : sig
  include Idl.RPC
    with type implementation = server_implementation
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) Result.result M.async
end
