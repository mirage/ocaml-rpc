module Old :
  sig
    type return_record = {
      result : string;
      metadata : (int * int) list;
      extras: string option;
    }
    val rpc_of_return_record : return_record -> Rpc.t
    val return_record_of_rpc : Rpc.t -> return_record
  end
type return_record_extended = {
  result : string;
  metadata : (int * int) list;
  extras : string option;
  new_field : string list;
}
val rpc_of_return_record_extended : return_record_extended -> Rpc.t
val default_return_record_extended : return_record_extended
val return_record_extended_of_rpc : Rpc.t -> return_record_extended
type variant = Foo of string list | Bar | Baz of float
val rpc_of_variant : variant -> Rpc.t
val variant_of_rpc : Rpc.t -> variant
module SubModule : sig  end
type __exn_ty2 = string
type __exn_ty1 = string * int * int
type __exn_ty0 = string
exception Unknown_RPC of __exn_ty2
exception Message_param_count_mismatch of __exn_ty1
exception Internal_error of __exn_ty0
module Exception :
  sig
    type exnty =
        Internal_error of string
      | Message_param_count_mismatch of (string * int * int)
      | Unknown_RPC of string
    val rpc_of_exnty : exnty -> Rpc.t
    val exnty_of_rpc : Rpc.t -> exnty
  end
val exnty_of_exn : exn -> Exception.exnty
val exn_of_exnty : Exception.exnty -> exn
module Args :
  sig
    module Rpc1 :
      sig
        type request = { arg1 : string; }
        val rpc_of_request : request -> Rpc.t
        val request_of_rpc : Rpc.t -> request
        val rpc_of___x2__ : int -> Rpc.t
        val __x2___of_rpc : Rpc.t -> int
        type response = Old.return_record
        val rpc_of_response : Old.return_record -> Rpc.t
        val response_of_rpc : Rpc.t -> Old.return_record
        val call_of_rpc1 : arg1:string -> int -> Rpc.call
      end
    module Rpc2 :
      sig
        type request = { opt : string option; }
        val rpc_of_request : request -> Rpc.t
        val request_of_rpc : Rpc.t -> request
        val rpc_of___x2__ : variant -> Rpc.t
        val __x2___of_rpc : Rpc.t -> variant
        type response = unit
        val rpc_of_response : 'a -> Rpc.t
        val response_of_rpc : Rpc.t -> unit
        val call_of_rpc2 : ?opt:string -> variant -> Rpc.call
      end
    module Rpc3 :
      sig
        val rpc_of___x1__ : int64 -> Rpc.t
        val __x1___of_rpc : Rpc.t -> int64
        type response = int64
        val rpc_of_response : int64 -> Rpc.t
        val response_of_rpc : Rpc.t -> int64
        val call_of_rpc3 : int64 -> Rpc.call
      end
    module SubModule :
      sig
        module Rpc4 :
          sig
            val rpc_of___x1__ : int64 -> Rpc.t
            val __x1___of_rpc : Rpc.t -> int64
            type response = int64
            val rpc_of_response : int64 -> Rpc.t
            val response_of_rpc : Rpc.t -> int64
            val call_of_rpc4 : int64 -> Rpc.call
          end
      end
  end
module type RPCM =
  sig
    type 'a t
    val rpc : Rpc.call -> Rpc.response t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : exn -> 'b t
  end
module type RPC = sig val rpc : Rpc.call -> Rpc.response end
module ClientM :
  functor (R : RPCM) ->
    sig
      val rpc1 : arg1:string -> int -> Old.return_record R.t
      val rpc2 : ?opt:string -> variant -> unit R.t
      val rpc3 : int64 -> int64 R.t
      module SubModule : sig val rpc4 : int64 -> int64 R.t end
    end
module Client :
  functor (R : RPC) ->
    sig
      module RPCM :
        sig
          type 'a t = 'a
          val rpc : Rpc.call -> Rpc.response
          val bind : 'a -> ('a -> 'b) -> 'b
          val return : 'a -> 'a
          val fail : exn -> 'a
        end
      val rpc1 : arg1:string -> int -> Old.return_record RPCM.t
      val rpc2 : ?opt:string -> variant -> unit RPCM.t
      val rpc3 : int64 -> int64 RPCM.t
      module SubModule : sig val rpc4 : int64 -> int64 RPCM.t end
    end
module type Server_impl =
  sig
    type context
    val rpc1 : context -> arg1:string -> int -> Old.return_record
    val rpc2 : context -> ?opt:string -> variant -> unit
    val rpc3 : context -> int64 -> int64
    module SubModule : sig val rpc4 : context -> int64 -> int64 end
  end
module type Server_implM =
  sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : exn -> 'a t
    val handle_failure : (unit -> 'b t) -> (exn -> 'b t) -> 'b t
    type context
    val rpc1 : context -> arg1:string -> int -> Old.return_record t
    val rpc2 : context -> ?opt:string -> variant -> unit t
    val rpc3 : context -> int64 -> int64 t
    module SubModule : sig val rpc4 : context -> int64 -> int64 t end
  end
module ServerM :
  functor (Impl : Server_implM) ->
    sig val process : Impl.context -> Rpc.call -> Rpc.response Impl.t end
module Server :
  functor (Impl : Server_impl) ->
    sig
      module ImplM :
        sig
          type 'a t = 'a
          val bind : 'a -> ('a -> 'b) -> 'b
          val return : 'a -> 'a
          val fail : exn -> 'a
          val handle_failure : (unit -> 'a) -> (exn -> 'a) -> 'a
          type context = Impl.context
          val rpc1 : context -> arg1:string -> int -> Old.return_record
          val rpc2 : context -> ?opt:string -> variant -> unit
          val rpc3 : context -> int64 -> int64
          module SubModule : sig val rpc4 : context -> int64 -> int64 end
        end
      module M :
        sig
          val process : ImplM.context -> Rpc.call -> Rpc.response ImplM.t
        end
      val process : ImplM.context -> Rpc.call -> Rpc.response ImplM.t
    end
