
module Param = struct
  type 'a t = {
    name : string option;
    description : string list;
    typedef : 'a Rpc.Types.def;
    version : Rpc.Version.t option;
  }

  type boxed = Boxed : 'a t -> boxed

  let mk ?name ?description ?version typedef =
    let description = match description with Some d -> d | None -> typedef.Rpc.Types.description in
    {name; description; version; typedef}

end

module Error = struct
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

  module Make(T : ERROR) = struct
    exception Exn of T.t
    let error = {
      def = T.t;
      raiser = (function e -> Exn e);
      matcher = (function | Exn e -> Some e | e -> T.internal_error_of e)
    }
  end
end

module Interface = struct
  type description = {
    name : string;
    namespace : string option;
    description : string list;
    version : Rpc.Version.t;
  }
end

module type RPC = sig
  type implementation
  type 'a res
  type ('a,'b) comp
  type _ fn

  val implement : Interface.description -> implementation

  val (@->) : 'a Param.t -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Param.t -> 'b Error.t -> ('a, 'b) comp fn
  val declare : string -> string list -> 'a fn -> 'a res
end

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val fail: exn -> 'a t
end

[@@@warning "-32-34"]
module type RPCTRANSFORMER = functor (M: MONAD) -> sig
  type _ m
  type 'a box = { box: 'a m }
  type ('a, 'b) resultb = ('a, 'b) Result.result box

  type rpcfn = Rpc.call -> Rpc.response M.t

  val box: 'a m -> 'a box
  val unbox: 'a box -> 'a m

  val lift: ('a -> 'b M.t) -> ('a -> 'b box)
  val bind:  'a box -> ('a -> 'b M.t) -> 'b box
  val return: 'a -> 'a box

  val run: 'a m -> 'a M.t

  val get: 'a box -> 'a M.t
  val (!@): 'a box -> 'a M.t
  val put: 'a M.t -> 'a box
  val (~@): 'a M.t -> 'a box
end
[@@@warning "+32+34"]


exception MarshalError of string
exception UnknownMethod of string
exception UnboundImplementation of string list

exception NoDescription

let get_wire_name description name =
  match description with
  | None -> name
  | Some d -> match d.Interface.namespace with
    | Some ns -> Printf.sprintf "%s.%s" ns name
    | None -> name

let get_arg call has_named name is_opt =
  match has_named, name, call.Rpc.params with
  | true, Some n, (Rpc.Dict named)::unnamed -> begin
      match List.partition (fun (x,_) -> x = n) named with
      | (_,arg)::dups,others when is_opt -> Result.Ok (Rpc.Enum [arg], {call with Rpc.params = (Rpc.Dict (dups @ others))::unnamed })
      | (_,arg)::dups,others -> Result.Ok (arg, {call with Rpc.params = (Rpc.Dict (dups @ others))::unnamed })
      | [], _others when is_opt -> Result.Ok (Rpc.Enum [], call)
      | _,_ -> Result.Error (`Msg (Printf.sprintf "Expecting named argument '%s'" n))
    end
  | true, None, (Rpc.Dict named)::unnamed -> begin
      match unnamed with
      | head::tail -> Result.Ok (head, {call with Rpc.params = (Rpc.Dict named)::tail})
      | _ -> Result.Error (`Msg "Incorrect number of arguments")
    end
  | true, _, _ -> begin
      Result.Error (`Msg "Marshalling error: Expecting dict as first argument when named parameters exist")
    end
  | false, None, head::tail -> begin
      Result.Ok (head, {call with Rpc.params = tail})
    end
  | false, None, [] ->
    Result.Error (`Msg "Incorrect number of arguments")
  | false, Some _, _ ->
    failwith "Can't happen by construction"

module Make (M: MONAD) = struct
  module T : sig
    type 'a box = { box: 'a M.t }
    type ('a, 'b) resultb = ('a, 'b) Result.result box

    type rpcfn = Rpc.call -> Rpc.response M.t

    val box: 'a M.t -> 'a box
    val unbox: 'a box -> 'a M.t

    val lift: ('a -> 'b M.t) -> ('a -> 'b box)
    val bind:  'a box -> ('a -> 'b M.t) -> 'b box
    val return: 'a -> 'a box

    val get: 'a box -> 'a M.t
    val (!@): 'a box -> 'a M.t
    val put: 'a M.t -> 'a box
    val (~@): 'a M.t -> 'a box
  end
  = struct
    type 'a box = { box: 'a M.t }
    type ('a, 'b) resultb = ('a, 'b) Result.result box

    type rpcfn = Rpc.call -> Rpc.response M.t

    let box x = { box=x }
    let unbox { box } = box

    let lift f = fun x -> {box=f x}
    let bind {box=x} f = {box=M.bind x f}
    let return x = {box=M.return x}

    let get {box=x} = x
    let (!@) = get
    let put x = {box=x}
    let (~@) = put
  end

  type client_implementation = unit
  type server_implementation = (string, T.rpcfn option) Hashtbl.t

  module ErrM: sig
    val return : 'a -> ('a, 'b) T.resultb
    val return_err : 'b -> ('a, 'b) T.resultb
    val checked_bind : ('a, 'b) T.resultb -> ('a -> ('c, 'd) T.resultb ) -> ('b -> ('c, 'd) T.resultb ) -> ('c, 'd) T.resultb
    val bind : ('a, 'b) T.resultb -> ('a -> ('c, 'b) T.resultb ) -> ('c, 'b) T.resultb
    val ( >>= ) : ('a, 'b) T.resultb -> ('a -> ('c, 'b) T.resultb ) -> ('c, 'b) T.resultb
  end = struct
    let return x = T.put (M.return (Result.Ok x))
    let return_err e = T.put (M.return (Result.Error e))
    let checked_bind x f f1 =
      T.bind x
        T.(function | Result.Ok x -> !@(f x) | Result.Error x -> !@(f1 x))
    let bind x f = checked_bind x f return_err
    let (>>=) x f = bind x f
  end

  module GenClient () =
  struct
    type implementation = client_implementation
    type 'a res = T.rpcfn -> 'a
    type ('a,'b) comp = ('a,'b) T.resultb
    type _ fn =
      | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
      | Returning : ('a Param.t * 'b Error.t) -> ('a, 'b) comp fn

    let description = ref None
    let implement x = description := Some x; ()

    let returning a err = Returning (a, err)
    let (@->) = fun t f -> Function (t, f)

    let declare name _ ty (rpc : T.rpcfn) =
      let open Result in
      let rec inner : type b. ((string * Rpc.t) list * Rpc.t list) -> b fn -> b = fun (named, unnamed) ->
        function
        | Function (t, f) -> begin
            fun v ->
              match t.Param.name with
              | Some n -> begin
                  match t.Param.typedef.Rpc.Types.ty, v with
                  | Rpc.Types.Option ty, Some v' ->
                    let marshalled = Rpcmarshal.marshal ty v' in
                    inner (((n,marshalled)::named),unnamed) f
                  | Rpc.Types.Option _ty, None ->
                    inner (named, unnamed) f
                  | ty, v ->
                    let marshalled = Rpcmarshal.marshal ty v in
                    inner (((n,marshalled)::named),unnamed) f
                end
              | None ->
                let marshalled = Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v in
                inner (named,(marshalled::unnamed)) f
          end
        | Returning (t, e) ->
          let wire_name = get_wire_name !description name in
          let args =
            match named with
            | [] -> List.rev unnamed
            | _ -> (Rpc.Dict named) :: List.rev unnamed
          in
          let call = Rpc.call wire_name args in
          let rpc = T.put (rpc call) in
          let res = T.bind rpc (fun r ->
              if r.Rpc.success
              then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> M.return (Ok x) | Error (`Msg x) -> M.fail (MarshalError x)
              else match Rpcmarshal.unmarshal e.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> M.return (Error x) | Error (`Msg x) -> M.fail  (MarshalError x))
          in res
      in inner ([],[]) ty
  end

  let server hashtbl =
    let impl = Hashtbl.create (Hashtbl.length hashtbl) in
    let unbound_impls = Hashtbl.fold (fun key fn acc ->
        match fn with
        | None -> key::acc
        | Some fn -> Hashtbl.add impl key fn; acc
      ) hashtbl [] in
    if unbound_impls <> [] then
      raise (UnboundImplementation unbound_impls);
    fun call ->
      let fn = try Hashtbl.find impl call.Rpc.name with Not_found -> raise (UnknownMethod call.Rpc.name) in
      fn call

  let combine hashtbls =
    let result = Hashtbl.create 16 in
    List.iter (Hashtbl.iter (fun k v -> Hashtbl.add result k v)) hashtbls;
    result

  module GenServer () = struct
    type implementation = server_implementation
    type ('a,'b) comp = ('a,'b) T.resultb
    type 'a res = 'a -> unit
    type _ fn =
      | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
      | Returning : ('a Param.t * 'b Error.t) -> ('a, 'b) comp fn

    let funcs = Hashtbl.create 20
    let description = ref None
    let implement x = description := Some x; funcs

    let returning a b = Returning (a,b)
    let (@->) = fun t f -> Function (t, f)

    let rec has_named_args : type a. a fn -> bool =
      function
      | Function (t, f) -> begin
          match t.Param.name with
          | Some _ -> true
          | None -> has_named_args f
        end
      | Returning (_, _) ->
        false

    let declare : string -> string list -> 'a fn -> 'a res = fun name _ ty ->
      (* We do not know the wire name yet as the description may still be unset *)
      Hashtbl.add funcs name None;
      fun impl ->
        begin
          (* Sanity check: ensure the description has been set before we declare
             any RPCs *)
          match !description with
          | Some _ -> ()
          (* TODO: use binds and M.fail in this case as well? *)
          | None -> raise NoDescription
        end;
        let rpcfn =
          let has_named = has_named_args ty in
          let rec inner : type a. a fn -> a -> T.rpcfn = fun f impl call ->
            match f with
            | Function (t, f) -> begin
                let is_opt = match t.Param.typedef.Rpc.Types.ty with | Rpc.Types.Option _ -> true | _ -> false in
                let (arg_rpc, call') =
                  match get_arg call has_named t.Param.name is_opt with
                  | Result.Ok (x,y) -> (x,y)
                  (* TODO: use binds and T.fail here, don't raise randomly *)
                  | Result.Error (`Msg m) -> raise (MarshalError m)
                in
                let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
                match z with
                | Result.Ok arg -> inner f (impl arg) call'
                (* TODO: use binds and T.fail here, don't raise randomly *)
                | Result.Error (`Msg m) -> raise (MarshalError m)
              end
            | Returning (t,e) ->
              T.bind impl (function
                  | Result.Ok x -> M.return (Rpc.success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty x))
                  | Result.Error y -> M.return (Rpc.failure (Rpcmarshal.marshal e.Error.def.Rpc.Types.ty y)))
              |> T.get
          in
          inner ty impl
        in

        Hashtbl.remove funcs name;
        (* The wire name might be different from the name *)
        let wire_name = get_wire_name !description name in
        Hashtbl.add funcs wire_name (Some rpcfn)
  end
end


module IdM = struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let fail e = raise e
end

module IdIdl = Make(IdM)
module GenClient = IdIdl.GenClient
let server = IdIdl.server
let combine = IdIdl.combine
module GenServer = IdIdl.GenServer


(* A default error variant as an example. In real code, this is more easily expressed by using the PPX:

      type default_error = InternalError of string [@@deriving rpcty]
*)
module DefaultError = struct
  type t = InternalError of string
  exception InternalErrorExn of string

  let internalerror : (string, t) Rpc.Types.tag = Rpc.Types.{
      tname="InternalError";
      tdescription=["Internal Error"];
      tversion=Some (1,0,0);
      tcontents=Basic String;
      tpreview = (function (InternalError s) -> Some s);
      treview = (fun s -> InternalError s)
    }

  (* And then we can create the 'variant' type *)
  let t : t Rpc.Types.variant = Rpc.Types.{
      vname    = "t";
      variants = [ BoxedTag internalerror ];
      vversion = Some (1,0,0);
      vdefault = Some (InternalError "Unknown error tag!");
      vconstructor = (fun s t ->
          match s with
          | "InternalError" -> Rresult.R.map (fun s -> internalerror.treview s) (t.tget (Basic String))
          | s -> Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s))}

  let def = Rpc.Types.{ name="default_error"; description=["Errors declared as part of the interface"]; ty=Variant t }

  let err = Error.{
      def = def;
      raiser = (function | InternalError s -> raise (InternalErrorExn s));
      matcher = function | InternalErrorExn s -> Some (InternalError s) | _ -> None
    }
end

module Exn = struct

  type rpcfn = Rpc.call -> Rpc.response
  type client_implementation = unit
  type server_implementation = (string, rpcfn option) Hashtbl.t

  module GenClient (R: sig val rpc: rpcfn end) =
  struct
    type implementation = client_implementation
    type ('a,'b) comp = 'a
    type 'a res = 'a
    type _ fn =
      | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
      | Returning : ('a Param.t * 'b Error.t) -> ('a,_) comp fn

    let description = ref None
    let implement x = description := Some x; ()

    let returning a err = Returning (a, err)
    let (@->) = fun t f -> Function (t, f)

    let declare name _ ty =
      let open Result in
      let rec inner : type b. ((string * Rpc.t) list * Rpc.t list) -> b fn -> b = fun (named,unnamed) ->
        function
        | Function (t, f) -> begin
            fun v ->
              match t.Param.name with
              | Some n -> begin
                  match t.Param.typedef.Rpc.Types.ty, v with
                  | Rpc.Types.Option ty, Some v' ->
                    let marshalled = Rpcmarshal.marshal ty v' in
                    inner (((n,marshalled)::named),unnamed) f
                  | Rpc.Types.Option _ty, None ->
                    inner (named, unnamed) f
                  | ty, v ->
                    let marshalled = Rpcmarshal.marshal ty v in
                    inner (((n,marshalled)::named),unnamed) f
                end
              | None ->
                let marshalled = Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v in
                inner (named,(marshalled::unnamed)) f
          end
        | Returning (t, e) ->
          let wire_name = get_wire_name !description name in
          let args =
            match named with
            | [] -> List.rev unnamed
            | _ -> (Rpc.Dict named) :: List.rev unnamed
          in
          let call = Rpc.call wire_name args in
          let r = R.rpc call in
          if r.Rpc.success
          then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> x | Error (`Msg x) -> raise (MarshalError x)
          else match Rpcmarshal.unmarshal e.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> raise (e.Error.raiser x) | Error (`Msg x) -> raise (MarshalError x)
      in inner ([],[]) ty
  end

  let server hashtbl =
    let impl = Hashtbl.create (Hashtbl.length hashtbl) in
    let unbound_impls = Hashtbl.fold (fun key fn acc ->
        match fn with
        | None -> key::acc
        | Some fn -> Hashtbl.add impl key fn; acc
      ) hashtbl [] in
    if unbound_impls <> [] then
      raise (UnboundImplementation unbound_impls);
    fun call ->
      let fn = try Hashtbl.find impl call.Rpc.name with Not_found -> raise (UnknownMethod call.Rpc.name) in
      fn call

  let combine hashtbls =
    let result = Hashtbl.create 16 in
    List.iter (Hashtbl.iter (fun k v -> Hashtbl.add result k v)) hashtbls;
    result

  module GenServer () = struct
    type implementation = server_implementation
    type ('a,'b) comp = 'a
    type 'a res = 'a -> unit
    type _ fn =
      | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
      | Returning : ('a Param.t * 'b Error.t) -> ('a, _) comp fn

    let funcs = Hashtbl.create 20
    let description = ref None
    let implement x = description := Some x; funcs

    let returning a b = Returning (a,b)
    let (@->) = fun t f -> Function (t, f)

    type boxed_error = BoxedError : 'a Error.t -> boxed_error

    let rec get_error_ty : type a. a fn -> boxed_error = function
      | Function (_,f) -> get_error_ty f
      | Returning (_,e) -> BoxedError e

    let rec has_named_args : type a. a fn -> bool =
      function
      | Function (t, f) -> begin
          match t.Param.name with
          | Some _ -> true
          | None -> has_named_args f
        end
      | Returning (_, _) ->
        false

    let declare : string -> string list -> 'a fn -> 'a res = fun name _ ty ->
      (* We do not know the wire name yet as the description may still be unset *)
      Hashtbl.add funcs name None;
      fun impl ->
        begin
          (* Sanity check: ensure the description has been set before we declare
             any RPCs *)
          match !description with
          | Some _ -> ()
          | None -> raise NoDescription
        end;
        let rpcfn =
          let has_named = has_named_args ty in
          let rec inner : type a. a fn -> a -> Rpc.call -> Rpc.response = fun f impl call ->
            try
              match f with
              | Function (t, f) ->
                let is_opt = match t.Param.typedef.Rpc.Types.ty with | Rpc.Types.Option _ -> true | _ -> false in
                let (arg_rpc, call') =
                  match get_arg call has_named t.Param.name is_opt with
                  | Result.Ok (x,y) -> (x,y)
                  | Result.Error (`Msg m) -> raise (MarshalError m)
                in
                let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
                let arg =
                  match z with
                  | Result.Ok arg -> arg
                  | Result.Error (`Msg m) -> raise (MarshalError m)
                in
                inner f (impl arg) call'
              | Returning (t,_) -> Rpc.success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty impl)
            with e ->
              let BoxedError error_ty = get_error_ty f in
              match error_ty.Error.matcher e with
              | Some y -> Rpc.failure (Rpcmarshal.marshal error_ty.Error.def.Rpc.Types.ty y)
              | None -> raise e
          in inner ty impl
        in

        Hashtbl.remove funcs name;
        (* The wire name might be different from the name *)
        let wire_name = get_wire_name !description name in
        Hashtbl.add funcs wire_name (Some rpcfn)

  end
end