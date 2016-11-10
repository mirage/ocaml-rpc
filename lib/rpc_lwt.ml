open Idl

(* Construct a helper monad to hide the nasty 'a comp type *)
module M = struct
  type 'a lwt = { lwt: 'a Lwt.t }
  type ('a, 'b) t = ('a, 'b) Result.result lwt

  let return x = { lwt=Lwt.return (Result.Ok x) }
  let return_err e = { lwt=Lwt.return (Result.Error e)}
  let checked_bind x f f1 = { lwt=Lwt.bind x.lwt (function | Result.Ok x -> (f x).lwt | Result.Error x -> (f1 x).lwt) }
  let bind x f = checked_bind x f return_err
  let (>>=) x f = bind x f
  let lwt x = x.lwt
end

module GenClient = struct
  type description = Interface.description
  let describe x = x

  exception MarshalError of string

  type ('a,'b) comp = ('a,'b) Result.result M.lwt
  type rpcfn = Rpc.call -> Rpc.response Lwt.t
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
        let res = Lwt.bind (rpc call) (fun r ->
          if r.Rpc.success
          then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> Lwt.return (Ok x) | Error (`Msg x) -> Lwt.fail (MarshalError x)
          else match Rpcmarshal.unmarshal e.Idl.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> Lwt.return (Error x) | Error (`Msg x) -> Lwt.fail (MarshalError x)) in
        {M.lwt=res}
    in inner [] ty
end

module GenServer = struct
  open Rpc

  type description = Idl.Interface.description
  let describe x = x

  exception MarshalError of string
  exception UnknownMethod of string

  type ('a,'b) comp = ('a,'b) Result.result M.lwt
  type rpcfn = Rpc.call -> Rpc.response Lwt.t
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
      | [Dict x] -> Lwt.return x
      | _ -> Lwt.fail (MarshalError "All arguments must be named currently")
    in

    let get_arg args name =
      if not (List.mem_assoc name args)
      then Lwt.fail (MarshalError (Printf.sprintf "Argument missing: %s" name))
      else Lwt.return (List.assoc name args)
    in

    let rpcfn =
      let rec inner : type a. a fn -> a -> call -> response Lwt.t = fun f impl call ->
        Lwt.bind (get_named_args call) (fun args ->
        match f with
        | Function (t, f) -> begin
            Lwt.bind (get_arg args t.Param.name) (fun arg_rpc ->
            let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
            match z with
            | Result.Ok arg -> inner f (impl arg) call
            | Result.Error (`Msg m) -> Lwt.fail (MarshalError m))
          end
        | Returning (t,e) -> begin
            Lwt.bind impl.M.lwt (function
                  | Result.Ok x -> Lwt.return (success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty x))
                  | Result.Error y -> Lwt.return (failure (Rpcmarshal.marshal e.Idl.Error.def.Rpc.Types.ty y)))
          end)
      in inner ty impl
    in

    Hashtbl.add functions name rpcfn;

    functions

  let server funcs call =
    let fn = try Hashtbl.find funcs call.name with Not_found -> raise (UnknownMethod call.name) in
    fn call
end
