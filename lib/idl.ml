module Param = struct
  type 'a t = {
    name : string;
    description : string;
    typedef : 'a Rpc.Types.def;
  }

  type boxed = Boxed : 'a t -> boxed

  let mk ?name ?description typedef =
    let name = match name with Some n -> n | None -> typedef.Rpc.Types.name in
    let description = match description with Some d -> d | None -> typedef.Rpc.Types.description in
    {name; description; typedef}

end

module Interface = struct 
  type description = {
    name : string;
    description : string;
    version : int;
  }
end

module type RPC = sig
  type description
  type 'a res
  type 'a comp
  type _ fn

  val describe : Interface.description -> description

  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'a comp fn
  val declare : string -> string -> 'a fn -> 'a res
end

let debug_rpc call =
  let str = Rpc.string_of_call call in
  Printf.printf "call: %s\n" str;
  let response = {Rpc.success=true; contents=Rpc.Int 7L} in
  Printf.printf "response: %s\n" (Rpc.string_of_response response);
  response

module GenClient = struct
  type description = Interface.description
  let describe x = x

  type 'a comp = 'a Rpc.Monad.error_or
  type rpcfn = Rpc.call -> Rpc.response Rpc.Monad.error_or
  type 'a res = rpcfn -> 'a
    
  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : 'a Param.t -> 'a comp fn

  let returning a = Returning a
  let (@->) = fun t f -> Function (t, f)

  let declare name _ ty (rpc : rpcfn) =
    let rec inner : type b. (string * Rpc.t) list -> b fn -> b = fun cur ->
      function
      | Function (t, f) ->
        fun v -> inner ((t.Param.name, Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v) :: cur) f
      | Returning t ->
        let call = Rpc.call name [(Rpc.Dict cur)] in
        Rpc.Monad.bind (rpc call) (fun response ->
        Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty response.Rpc.contents)
    in inner [] ty   
end

module GenServer = struct
  open Rpc

  type description = Interface.description
  let describe x = x
      
  type 'a comp = 'a
  type rpcfn = Rpc.call -> Rpc.response Rpc.Monad.error_or
  type funcs = (string, rpcfn) Hashtbl.t
  type 'a res = 'a -> funcs -> funcs
    
  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : 'a Param.t -> 'a comp fn
        
  let returning a = Returning a
  let (@->) = fun t f -> Function (t, f)

  let empty : unit -> funcs = fun () -> Hashtbl.create 20
  
  let declare name _ ty impl functions =
    let open Rpc.Monad in
    let get_named_args call =
      match call.params with
      | [Dict x] -> return x
      | _ -> error_of_string "All arguments must be named currently"
    in

    let get_arg args name =
      if not (List.mem_assoc name args)
      then (error_of_string "Argument missing")
      else (return (List.assoc name args))
    in

    let rpcfn =
      let rec inner : type a. a fn -> a -> call -> response Monad.error_or = fun f impl call ->
        get_named_args call >>= fun args ->
        match f with
        | Function (t, f) ->
          get_arg args t.Param.name >>= fun arg_rpc ->
          Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc >>= fun arg ->
          inner f (impl arg) call
        | Returning t ->
          return (success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty impl))
      in inner ty impl
    in

    Hashtbl.add functions name rpcfn;

    functions

  let server funcs call : response Monad.error_or =
    try
      let fn = Hashtbl.find funcs call.name in
      fn call
    with _ ->
      Rpc.Monad.error_of_string "Unknown RPC"    
end
