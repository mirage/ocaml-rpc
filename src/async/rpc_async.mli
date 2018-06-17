module AsyncM : Idl.RPCMONAD

(** Client generator similar to {!Idl.GenClient} that uses [Async]. *)
module GenClient () :
  sig
    open AsyncM
    include Idl.RPC
      with type implementation = unit
       and type 'a res = rpcfn -> 'a
       and type ('a,'b) comp = ('a,'b) t
  end

module ServerImpl : sig
  open AsyncM
  type server_implementation = (string, rpcfn option) Hashtbl.t
  val server : server_implementation -> rpcfn
  val combine : server_implementation list -> server_implementation
end

(** Server generator similar to {!Idl.GenServer} that uses [Async]. *)
module GenServer () : sig
  open AsyncM
  include Idl.RPC
    with type implementation = (string, rpcfn option) Hashtbl.t
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) t
end
