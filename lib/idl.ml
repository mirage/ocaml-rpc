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
  type ('a, 'b) comp
  type _ fn

  val describe : Interface.description -> description

  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : ('a Param.t * 'b Param.t) -> ('a, 'b) comp fn
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

  type ('a, 'b) comp = ('a, 'b) Result.result
  type 'a rpcfn = Rpc.call -> (Rpc.response, 'a) Result.result
  type ('a, 'b) res = ('b rpcfn) -> 'a

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * ([> `Msg of string] as 'b) Param.t) -> ('a, 'b) comp fn

  let returning a err = Returning (a, err)
  let (@->) = fun t f -> Function (t, f)

  let declare name _ ty (rpc : 'c rpcfn) =
    let rec inner : type b. (string * Rpc.t) list -> b fn -> b = fun cur ->
      function
      | Function (t, f) ->
        fun v -> inner ((t.Param.name, Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v) :: cur) f
      | Returning (t, e) ->
        let call = Rpc.call name [(Rpc.Dict cur)] in
        match (rpc call) with
        | Result.Ok r -> begin
          if r.Rpc.success
          then
            let x = Rresult.R.open_error_msg (Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents)
            in x
          else
            let y = Rpcmarshal.unmarshal e.Param.typedef.Rpc.Types.ty r.Rpc.contents in
            match y  with
            | Result.Ok x -> Result.Error x
            | Result.Error y -> Rresult.R.open_error_msg (Result.Error y)
          end
        | Result.Error y -> Rresult.R.open_error_msg (Result.Error y)
    in inner [] ty
end

module GenServer = struct
  open Rpc

  type description = Interface.description
  let describe x = x

  type ('a, 'b) comp = ('a, 'b) Result.result
  type 'a rpcfn = Rpc.call -> (Rpc.response, [> `Msg of string] as 'a) Result.result
  type 'a funcs = (string, 'a rpcfn) Hashtbl.t
  type ('a, 'b) res = 'a -> 'b funcs -> 'b funcs

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * ([> `Msg of string] as 'b) Param.t) -> ('a, 'b) comp fn

  let returning a = Returning a
  let (@->) = fun t f -> Function (t, f)

  let empty : unit -> 'a funcs = fun () -> Hashtbl.create 20

  let declare : string -> string -> 'a fn -> ('a, [> `Msg of string] as 'b) res = fun name _ ty impl functions ->
    let open Rresult.R in
    let get_named_args call =
      match call.params with
      | [Dict x] -> return x
      | _ -> error_msg "All arguments must be named currently"
    in

    let get_arg args name =
      if not (List.mem_assoc name args)
      then (error_msg "Argument missing")
      else (return (List.assoc name args))
    in

    let rpcfn =
      let rec inner : type a. a fn -> a -> call -> (response, [> Rresult.R.msg]) Result.result = fun f impl call ->
        let x = open_error_msg (get_named_args call) in
        x >>= fun args ->
        match f with
        | Function (t, f) ->
          let y = get_arg args t.Param.name in
          y >>= fun arg_rpc ->
          let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
          open_error_msg z >>= fun arg ->
          inner f (impl arg) call
        | Returning (t,e) -> begin
          match impl with
          | Result.Ok x -> open_error_msg (return (success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty x)))
          | Result.Error y -> open_error_msg (return (failure (Rpcmarshal.marshal e.Param.typedef.Rpc.Types.ty y)))
          end
      in inner ty impl
    in

    Hashtbl.add functions name rpcfn;

    functions

  let server funcs call : (response, [> Rresult.R.msg]) Result.result =
    try
      let fn = Hashtbl.find funcs call.name in
      fn call
    with _ ->
      Rresult.R.error_msg "Unknown RPC"
end
