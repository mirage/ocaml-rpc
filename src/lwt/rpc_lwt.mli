module LwtM : Idl.RPCMONAD

(** Client generator similar to {!Idl.GenClient} that uses [Lwt]. *)
module GenClient () :
  sig
    open LwtM
    include Idl.RPC
      with type implementation = unit
       and type 'a res = rpcfn -> 'a
       and type ('a,'b) comp = ('a,'b) t
  end

module ServerImpl : sig
  open LwtM
  type server_implementation
  val server : server_implementation -> rpcfn
  val combine : server_implementation list -> server_implementation
end

(** Server generator similar to {!Idl.GenServer} that uses [Lwt]. *)
module GenServer () : sig
  open LwtM
  include Idl.RPC
    with type implementation = (string, rpcfn option) Hashtbl.t
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) t
end
