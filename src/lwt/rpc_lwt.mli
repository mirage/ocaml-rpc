module LwtM : Idl.RPCMONAD

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
