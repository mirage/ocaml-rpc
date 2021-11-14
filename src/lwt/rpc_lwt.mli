type client_implementation
type server_implementation

module T : sig
  type 'a box
  type ('a, 'b) resultb = ('a, 'b) Result.t box
  type rpcfn = Rpc.call -> Rpc.response Lwt.t

  val lift : ('a -> 'b Lwt.t) -> 'a -> 'b box
  val bind : 'a box -> ('a -> 'b Lwt.t) -> 'b box
  val return : 'a -> 'a box
  val get : 'a box -> 'a Lwt.t
  val ( !@ ) : 'a box -> 'a Lwt.t
  val put : 'a Lwt.t -> 'a box
  val ( ~@ ) : 'a Lwt.t -> 'a box
end

module ErrM : sig
  val return : 'a -> ('a, 'b) T.resultb
  val return_err : 'b -> ('a, 'b) T.resultb

  val checked_bind
    :  ('a, 'b) T.resultb
    -> ('a -> ('c, 'd) T.resultb)
    -> ('b -> ('c, 'd) T.resultb)
    -> ('c, 'd) T.resultb

  val bind : ('a, 'b) T.resultb -> ('a -> ('c, 'b) T.resultb) -> ('c, 'b) T.resultb
  val ( >>= ) : ('a, 'b) T.resultb -> ('a -> ('c, 'b) T.resultb) -> ('c, 'b) T.resultb
end

(** Client generator similar to {!Idl.GenClient} that uses [Lwt]. *)
module GenClient () : sig
  include
    Idl.RPC
      with type implementation = client_implementation
       and type 'a res = T.rpcfn -> 'a
       and type ('a, 'b) comp = ('a, 'b) T.resultb
end

(** Server generator similar to {!Idl.GenServer} that uses [Lwt]. *)
module GenServer () : sig
  include
    Idl.RPC
      with type implementation = server_implementation
       and type 'a res = 'a -> unit
       and type ('a, 'b) comp = ('a, 'b) T.resultb
end

val server : server_implementation -> T.rpcfn
val combine : server_implementation list -> server_implementation
