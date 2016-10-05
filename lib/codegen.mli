type 'a comp = 'a
type _ fn =
    Function : 'a Idl.Param.t * 'b fn -> ('a -> 'b) fn
  | Returning : 'a Idl.Param.t -> 'a comp fn
module Method :
  sig
    type 'a t = { name : string; description : string; ty : 'a fn; }
    val find_inputs : 'a fn -> Idl.Param.boxed list
    val find_output : 'a fn -> Idl.Param.boxed
  end
type boxed_fn = BoxedFunction : 'a Method.t -> boxed_fn
module Interface :
  sig
    type description =
      Idl.Interface.description = {
      name : string;
      description : string;
      version : Rpc.Version.t;
    }
    type t = {
      details : Idl.Interface.description;
      methods : boxed_fn list;
    }
    val prepend_arg : t -> 'a Idl.Param.t -> t
    val all_types : t -> Rpc.Types.boxed_def list
  end
type description = Interface.t
val describe : Interface.description -> Interface.t
module Interfaces :
  sig
    type t = {
      name : string;
      title : string;
      description : string;
      type_decls : Rpc.Types.boxed_def list;
      interfaces : Interface.t list;
      exn_decls : Rpc.Types.boxed_def;
    }
    val empty : string -> string -> string -> t
    val register_exn : t -> 'a Rpc.Types.def -> t
    val add_interface : t -> Interface.t -> t
  end
type 'a res = Interface.t -> Interface.t
val returning : 'a Idl.Param.t -> 'a comp fn
val ( @-> ) : 'a Idl.Param.t -> 'b fn -> ('a -> 'b) fn
val declare : string -> string -> 'a fn -> Interface.t -> Interface.t
