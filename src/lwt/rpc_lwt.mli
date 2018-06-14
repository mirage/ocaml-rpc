type lwt_rpcfn = Rpc.call -> Rpc.response Lwt.t

module M :
  sig
    type 'a lwt = { lwt : 'a Lwt.t; }
    type ('a, 'b) t = ('a, 'b) Result.result lwt
    val return : 'a -> ('a, 'b) t
    val return_err : 'b -> ('a, 'b) t
    val checked_bind : ('a, 'b) t -> ('a -> ('c, 'd) t) -> ('b -> ('c, 'd) t) -> ('c, 'd) t
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
    val lwt : 'a lwt -> 'a Lwt.t
  end

(** Client generator similar to {!Idl.GenClient} that uses [Lwt]. *)
module GenClient () :
  sig
    include Idl.RPC
      with type implementation = unit
       and type 'a res = lwt_rpcfn -> 'a
       and type ('a,'b) comp = ('a,'b) Result.result M.lwt
  end

type server_implementation
val server : server_implementation -> lwt_rpcfn
val combine : server_implementation list -> server_implementation

(** Server generator similar to {!Idl.GenServer} that uses [Lwt]. *)
module GenServer () : sig
  include Idl.RPC
    with type implementation = server_implementation
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) Result.result M.lwt
end
