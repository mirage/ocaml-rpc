open Idl
module Deferred = Async.Std.Deferred

(* Construct a helper monad to hide the nasty 'a comp type *)
module M = struct
  type 'a async = { async: 'a Deferred.t }
  type ('a, 'b) t = ('a, 'b) Result.result async

  let return x = { async=Deferred.return (Result.Ok x) }
  let return_err e = { async=Deferred.return (Result.Error e)}
  let checked_bind x f f1 = { async=Deferred.bind x.async (function | Result.Ok x -> (f x).async | Result.Error x -> (f1 x).async) }
  let bind x f = checked_bind x f return_err
  let (>>=) x f = bind x f
  let deferred x = x.async
end

module GenClient = struct
  type description = Interface.description
  let describe x = x

  exception MarshalError of string

  type ('a,'b) comp = ('a,'b) Result.result M.async
  type rpcfn = Rpc.call -> Rpc.response Deferred.t
  type 'a res = rpcfn -> 'a

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Idl.Error.t) -> ('a, 'b) M.t fn

  let returning a err = Returning (a, err)
  let (@->) = fun t f -> Function (t, f)

  let declare name _ ty (rpc : rpcfn) =
    let open Result in
    let rec inner : type b. (string * Rpc.t) list -> b fn -> b = fun cur ->
      function
      | Function (t, f) ->
        fun v -> inner ((t.Param.name, Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v) :: cur) f
      | Returning (t, e) ->
        let call = Rpc.call name [(Rpc.Dict cur)] in
        let res = Deferred.bind (rpc call) (fun r ->
          if r.Rpc.success
          then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> Deferred.return (Ok x) | Error (`Msg x) -> raise (MarshalError x)
          else match Rpcmarshal.unmarshal e.Idl.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> Deferred.return (Error x) | Error (`Msg x) -> raise (MarshalError x)) in
        {M.async=res}
    in inner [] ty
end

module GenServer = struct
  open Rpc

  type description = Idl.Interface.description
  let describe x = x

  exception MarshalError of string
  exception UnknownMethod of string

  type ('a,'b) comp = ('a,'b) Result.result M.async
  type rpcfn = Rpc.call -> Rpc.response Deferred.t
  type funcs = (string, rpcfn) Hashtbl.t
  type 'a res = 'a -> funcs -> funcs

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Idl.Error.t) -> ('a, 'b) M.t fn

  let returning a b = Returning (a,b)
  let (@->) = fun t f -> Function (t, f)

  let empty : unit -> funcs = fun () -> Hashtbl.create 20

  let declare : string -> string -> 'a fn -> 'a res = fun name _ ty impl functions ->
    let open Rresult.R in
    let get_named_args call =
      match call.params with
      | [Dict x] -> x
      | _ -> raise (MarshalError "All arguments must be named currently")
    in

    let get_arg args name =
      if not (List.mem_assoc name args)
      then raise (MarshalError (Printf.sprintf "Argument missing: %s" name))
      else List.assoc name args
    in

    let rpcfn =
      let rec inner : type a. a fn -> a -> call -> response Deferred.t = fun f impl call ->
        let args = get_named_args call in
        match f with
        | Function (t, f) -> begin
            let arg_rpc = get_arg args t.Param.name in
            let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
            match z with
            | Result.Ok arg -> inner f (impl arg) call
            | Result.Error (`Msg m) -> raise (MarshalError m)
          end
        | Returning (t,e) -> begin
            Deferred.bind impl.M.async (function
                  | Result.Ok x -> Deferred.return (success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty x))
                  | Result.Error y -> Deferred.return (failure (Rpcmarshal.marshal e.Idl.Error.def.Rpc.Types.ty y)))
          end
      in inner ty impl
    in

    Hashtbl.add functions name rpcfn;

    functions

  let server funcs call =
    let fn = try Hashtbl.find funcs call.name with Not_found -> raise (UnknownMethod call.name) in
    fn call
end
