module AsyncM : Idl.RPCMONAD

module M : sig
  val return : 'a -> ('a, 'b) AsyncM.t
  val return_err : 'b -> ('a, 'b) AsyncM.t
  val checked_bind : ('a, 'b) AsyncM.t -> ('a -> ('c, 'd) AsyncM.t) -> ('b -> ('c, 'd) AsyncM.t) -> ('c, 'd) AsyncM.t
  val bind : ('a, 'b) AsyncM.t -> ('a -> ('c, 'b) AsyncM.t) -> ('c, 'b) AsyncM.t
  val ( >>= ) : ('a, 'b) AsyncM.t -> ('a -> ('c, 'b) AsyncM.t) -> ('c, 'b) AsyncM.t
  val deferred : 'a AsyncM.box -> 'a Async.Deferred.t
end

type client_implementation
type server_implementation

(** Client generator similar to {!Idl.GenClient} that uses [Async]. *)
module GenClient () :
sig
  open AsyncM
  include Idl.RPC
    with type implementation = client_implementation
     and type 'a res = rpcfn -> 'a
     and type ('a,'b) comp = ('a,'b) t
end

val server : server_implementation -> AsyncM.rpcfn
val combine : server_implementation list -> server_implementation

(** Server generator similar to {!Idl.GenServer} that uses [Async]. *)
module GenServer () : sig
  open AsyncM
  include Idl.RPC
    with type implementation = server_implementation
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) t
end
