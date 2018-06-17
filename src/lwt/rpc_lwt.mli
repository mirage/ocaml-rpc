module LwtM : Idl.RPCMONAD

module M : sig
  val return : 'a -> ('a, 'b) LwtM.t
  val return_err : 'b -> ('a, 'b) LwtM.t
  val checked_bind : ('a, 'b) LwtM.t -> ('a -> ('c, 'd) LwtM.t) -> ('b -> ('c, 'd) LwtM.t) -> ('c, 'd) LwtM.t
  val bind : ('a, 'b) LwtM.t -> ('a -> ('c, 'b) LwtM.t) -> ('c, 'b) LwtM.t
  val ( >>= ) : ('a, 'b) LwtM.t -> ('a -> ('c, 'b) LwtM.t) -> ('c, 'b) LwtM.t
  val lwt : 'a LwtM.box -> 'a Lwt.t
end

type client_implementation
type server_implementation

(** Client generator similar to {!Idl.GenClient} that uses [Lwt]. *)
module GenClient () :
sig
  open LwtM
  include Idl.RPC
    with type implementation = client_implementation
     and type 'a res = rpcfn -> 'a
     and type ('a,'b) comp = ('a,'b) t
end

val server : server_implementation -> LwtM.rpcfn
val combine : server_implementation list -> server_implementation

(** Server generator similar to {!Idl.GenServer} that uses [Lwt]. *)
module GenServer () : sig
  open LwtM
  include Idl.RPC
    with type implementation = server_implementation
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) t
end
