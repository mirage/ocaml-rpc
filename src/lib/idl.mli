(** The Idl module is for declaring the types and documentation for RPC calls *)

(** The Param module is associated with parameters to RPCs. RPCs are defined in terms of
    'a Param.t values. *)
module Param : sig

  (** A Param.t has a name, description and a typedef. We may also want to add in here
      default values, example values and so on *)
  type 'a t = {
    name : string option;
    description : string list;
    typedef : 'a Rpc.Types.def;
    version : Rpc.Version.t option;
  }

  (** We box parameters to put them into lists *)
  type boxed = Boxed : 'a t -> boxed

  (** [mk ~name ~description typ] creates a Param.t out of a type definition
      from the Types module. If the name or description are omitted, the name
      or description from the type definition will be inherited *)
  val mk : ?name:string -> ?description:string list ->
    ?version:Rpc.Version.t -> 'a Rpc.Types.def -> 'a t
end

(* An error that might be raised by an RPC *)
module Error : sig
  type 'a t = {
    def : 'a Rpc.Types.def;
    raiser : 'a -> exn;
    matcher : exn -> 'a option;
  }

  module type ERROR = sig
    type t
    val t : t Rpc.Types.def
    val internal_error_of: exn -> t option
  end

  module Make(T : ERROR) : sig
    val error : T.t t
  end
end

(** An interface is a collection of RPC declarations. *)
module Interface : sig
  type description = {
    name : string;
    namespace : string option;
    description : string list;
    version : Rpc.Version.t;
  }
end

(** Raised when the client/server failed to unmarshal the RPC response/request. *)
exception MarshalError of string

val get_wire_name : Interface.description option -> string -> string

val get_arg : Rpc.call -> bool -> string option -> bool -> (Rpc.t * Rpc.call, [> `Msg of string]) result

(** The RPC module type is the standard module signature that the various
    specialization modules must conform to. *)
module type RPC = sig

  (** The implementation is dependent on the module, and represents the
      'result' of the entire module. For example, in the Server
      module, the `implementation` is the server function, with type

          Rpc.call -> Rpc.response.

      For the Client module, the individual declarations are used to perform
      the RPCs, and the 'implementation' type is simply unit. *)
  type implementation

  (** To actually construct the implementation, an interface description
      must be provided *)
  val implement : Interface.description -> implementation

  (** 'a res is the result type of declaring a function. For example,
      the Client module, given an (int -> int -> int) fn, will return
      a function of type 'a - in this case, (int -> int -> int) *)
  type 'a res

  (** This is for inserting a type in between the function application
      and its result. For example, this could be an Lwt.t, meaning that
      the result of a function application is a thread *)
  type ('a,'b) comp

  (** The GADT specifying the type of the RPC *)
  type _ fn

  (** This infix operator is for constructing function types *)
  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn

  (** This defines the return type of an RPC *)
  val returning : 'a Param.t -> 'b Error.t -> ('a, 'b) comp fn

  (** [declare name description typ] is how an RPC is declared to the
      module implementing the functionality. The return type is dependent
      upon the module being used *)
  val declare : string -> string list -> 'a fn -> 'a res
end

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail: exn -> 'a t
end


(** For the Server generation, the 'implement' function call _must_ be called
    before any RPCs are described. This exception will be raised if the user
    tries to do this. *)
exception NoDescription

module Make (M: MONAD): sig

  type client_implementation
  type server_implementation

  (** The module [!T], the [RPC] [MONAD] transformer, defines the minimal set
    of types and functions needed for the [!GenClient] and [!GenServer] modules
    to generate clients and servers. These allow to provide different syncronous
    and asynctronous engines for the client and server implementations.
*)
  module T: sig
    type _ m
    type 'a box = { box: 'a m }
    type ('a, 'b) resultb = ('a, 'b) Result.result box

    type rpcfn = Rpc.call -> Rpc.response m

    val box: 'a m -> 'a box
    val unbox: 'a box -> 'a m
    val lift: ('a -> 'b M.t) -> ('a -> 'b m)
    val bind:  'a m -> ('a -> 'b M.t) -> 'b m
    val return: 'a -> 'a m

    val run: 'a m -> 'a M.t
  end

  (** [!ErrM] defines monad to use for the implementation and combination
      of RPC functions *)
  module ErrM: sig
    val return : 'a -> ('a, 'b) T.resultb
    val return_err : 'b -> ('a, 'b) T.resultb
    val checked_bind : ('a, 'b) T.resultb -> ('a -> ('c, 'd) T.resultb ) -> ('b -> ('c, 'd) T.resultb ) -> ('c, 'd) T.resultb
    val bind : ('a, 'b) T.resultb -> ('a -> ('c, 'b) T.resultb ) -> ('c, 'b) T.resultb
    val ( >>= ) : ('a, 'b) T.resultb -> ('a -> ('c, 'b) T.resultb ) -> ('c, 'b) T.resultb
  end

  (** This module generates Client modules from RPC declarations.

      The [implementation] of the module as a whole is unused. However, the
      namespace comes from the interface description, and hence calling
      [implement] is important.

      The result of declaring a function of type 'a (where for example 'a might
      be [(int -> string -> (bool, err) result)]), is a function that takes an rpc
      function, which might send the RPC across the network, and returns a
      function of type 'a, in this case [(int -> string -> (bool, err) result)].

      Our functions return a [Result.result] type, which contains
      the result of the Rpc, which might be an error message indicating
      a problem happening on the remote end. *)
  module GenClient (): sig
    include RPC
      with type implementation = client_implementation
       and type 'a res = T.rpcfn -> 'a
       and type ('a,'b) comp = ('a,'b) T.resultb
  end

  (** This module generates a server that dispatches RPC calls to their
      implementations.

      Given an {!Rpc.call}, it calls the implementation of that RPC method and performs
      the marshalling and unmarshalling. It is up to the user of this library to
      connect this rpc function to a real server that responds to client requests.

      The implementations of each RPC method should be specified by passing it to
      the corresponding function in the generated module.

      Then the server itself can be obtained by passing the [implementation]
      to {!server}. *)
  module GenServer (): sig
    include RPC
      with type implementation = server_implementation
       and type 'a res = 'a -> unit
       and type ('a,'b) comp = ('a,'b) T.resultb
  end

  val server : server_implementation -> T.rpcfn
  val combine : server_implementation list -> server_implementation
end

module IdM: MONAD
module GenClient () : sig
  open IdM
  include RPC
    with type implementation = Make(IdM).client_implementation
     and type 'a res = IdM.rpcfn -> 'a
     and type ('a,'b) comp = ('a,'b) t
end
module GenServer () : sig
  open IdM
  include RPC
    with type implementation = Make(IdM).server_implementation
     and type 'a res = 'a -> unit
     and type ('a,'b) comp = ('a,'b) t
end
val server : Make(IdM).server_implementation -> IdM.rpcfn
val combine : Make(IdM).server_implementation list -> Make(IdM).server_implementation


module DefaultError : sig
  type t = InternalError of string
  exception InternalErrorExn of string

  val internalerror : (string, t) Rpc.Types.tag
  val t : t Rpc.Types.variant
  val def : t Rpc.Types.def
  val err : t Error.t
end


module Legacy : sig
  type rpcfn = Rpc.call -> Rpc.response

  (** This module generates exception-raising Client modules from RPC
      declarations. See the {!GenClient} module for a description of
      the common entries. *)
  module GenClientExn () : sig
    include RPC
      with type implementation = unit
       and type 'a res = rpcfn -> 'a
       (* Our functions never return the error parameter, hence the following
          type declaration drops the `b parameter. Instead, the exception declared
          in the Error.t passed in the `returning` function below will be raised.  *)
       and type ('a,'b) comp = 'a
  end

  module type RPCfunc = sig
    val rpc : Rpc.call -> Rpc.response
  end

  (** This module is like {!GenClientExn}, but it allows the user to specify the
      [rpc] function once when generating the client. *)
  module GenClientExnRpc (R : RPCfunc) : sig
    include RPC
      with type implementation = unit
       and type 'a res = 'a
       and type ('a,'b) comp = 'a
  end

  type server_implementation
  val server : server_implementation -> rpcfn
  val combine : server_implementation list -> server_implementation

  (** Generates a server, like {!GenServer}, but for an implementation that
      raises exceptions instead of returning a [result]. *)
  module GenServerExn () : sig
    include RPC
      with type implementation = server_implementation
       and type 'a res = 'a -> unit
       and type ('a,'b) comp = 'a
  end
end
