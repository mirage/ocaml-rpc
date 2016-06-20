(** The Idl module is for declaring the types and documentation for RPC calls *)

(** The Param module is associated with parameters to RPCs. RPCs are defined in terms of 
    'a Param.t values. *)
module Param : sig

  (** A Param.t has a name, description and a typedef. We may also want to add in here
      default values, example values and so on *)
  type 'a t = {
    name : bytes;
    description : bytes;
    typedef : 'a Types.def;
  }

  (** We box parameters to put them into lists *)
  type boxed = Boxed : 'a t -> boxed

  (** [mk ~name ~description typ] creates a Param.t out of a type definition
      from the Types module. If the name or description are omitted, the name
      or description from the type definition will be inherited *)
  val mk : ?name:string -> ?description:string -> 'a Types.def -> 'a t
end

(** An interface is a collection of RPC declarations. *)
module Interface : sig
  type description = {
    name : string;
    description : string;
    version : int;
  }
end

(** The RPC module type is the standard module signature that the various
    specialization modules must conform to. *)   
module type RPC = sig
  
  (** The description is dependent on the module. *)
  type description
  val describe : Interface.description -> description

  (** 'a res is the result type of declaring a function. For example,
      the Client module, given an (int -> int -> int) fn, will return
      a function of type 'a - in this case, (int -> int -> int) *)
  type 'a res

  (** This is for inserting a type in between the function application
      and its result. For example, this could be an Lwt.t, meaning that
      the result of a function application is a thread *)
  type 'a comp

  (** The GADT specifying the type of the RPC *)
  type _ fn

  (** This infix operator is for constructing function types *)
  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn

  (** This defines the return type of an RPC *)
  val returning : 'a Param.t -> 'a comp fn

  (** [declare name description typ] is how an RPC is declared to the
      module implementing the functionality. The return type is dependent
      upon the module being used *)
  val declare : string -> string -> 'a fn -> 'a res
end


(** This module generates Client modules from RPC declarations *)
module GenClient : sig
  type description = Interface.description
  val describe : Interface.description -> description

  (** The result of declaring a function of type 'a (where for example
      'a might be (int -> string -> bool)), is a function that takes
      an rpc function, which might send the RPC across the network,
      and returns a function of type 'a, in this case (int -> string
      -> bool). *)
  type 'a res = (Rpc.call -> Rpc.response Rpc.error_or) -> 'a

  (** Our functions return a Result.result type, which either contains
      the result of the Rpc, or an error message indicating a problem
      happening at the transport level *)
  type 'a comp = 'a Rpc.error_or

  type _ fn
  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end

module GenServer : sig
  type description = Interface.description
  val describe : Interface.description -> description

  (** 'funcs' is a Hashtbl type that is used to hold the implementations of 
      the RPCs *)
  type funcs = (string, Rpc.call -> Rpc.response Rpc.error_or) Hashtbl.t

  (** The result of a declaration of an RPC is a function type that takes
      as first parameter the implementation of the RPC, and as second
      parameter the 'funcs' object that is accumulating each implementation.
      When all the implementations have been added to the 'funcs' object,
      it can be passed to the 'server' function below *)
  type 'a res = 'a -> funcs -> funcs

  (** No error handling done server side yet *)
  type 'a comp = 'a
  type _ fn

  (** The 'empty' method here can be used to obtain an initial 'funcs' object
      to accumulate the implementations in *)
  val empty : unit -> funcs

  (** [server funcs] is a function that can be used to respond to RPC calls *)
  val server : funcs -> Rpc.call -> Rpc.response Rpc.error_or

  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end


