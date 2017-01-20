
module Param = struct
  type 'a t = {
    name : string;
    description : string list;
    typedef : 'a Rpc.Types.def;
    version : Rpc.Version.t option;
  }

  type boxed = Boxed : 'a t -> boxed

  let mk ?name ?description ?version typedef =
    let name = match name with Some n -> n | None -> typedef.Rpc.Types.name in
    let description = match description with Some d -> d | None -> typedef.Rpc.Types.description in
    {name; description; version; typedef}

end

module Error = struct
  type 'a t = {
    def : 'a Rpc.Types.def;
    raiser : 'a -> exn;
    matcher : exn -> 'a option;
  }

  module Make(T : sig type t val t : t Rpc.Types.def end) = struct
    exception Exn of T.t
    let error = {
        def = T.t;
        raiser = (function e -> Exn e);
        matcher = (function | Exn e -> Some e | _ -> None)
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

let debug_rpc call =
  let str = Rpc.string_of_call call in
  Printf.printf "call: %s\n" str;
  let response = {Rpc.success=true; contents=Rpc.Int 7L} in
  Printf.printf "response: %s\n" (Rpc.string_of_response response);
  response

exception MarshalError of string
exception UnknownMethod of string

let get_wire_name description name =
  match description with
  | None -> name
  | Some d -> match d.Interface.namespace with
    | Some ns -> Printf.sprintf "%s.%s" ns name
    | None -> name

let get_named_args call =
  match call.Rpc.params with
  | [Rpc.Dict x] -> x
  | _ -> raise (MarshalError "All arguments must be named currently")

let get_arg args name =
  if not (List.mem_assoc name args)
  then raise (MarshalError (Printf.sprintf "Argument missing: %s" name))
  else List.assoc name args

type client_implementation = unit

module GenClient () =
struct
  type implementation = client_implementation

  let description = ref None
  let implement x = description := Some x; ()

  type ('a,'b) comp = ('a,'b) Result.result
  type rpcfn = Rpc.call -> Rpc.response
  type 'a res = rpcfn -> 'a

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Error.t) -> ('a, 'b) comp fn

  let returning a err = Returning (a, err)
  let (@->) = fun t f -> Function (t, f)

  let declare name _ ty (rpc : rpcfn) =
    let open Result in
    let rec inner : type b. (string * Rpc.t) list -> b fn -> b = fun cur ->
      function
      | Function (t, f) ->
        fun v -> inner ((t.Param.name, Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v) :: cur) f
      | Returning (t, e) ->
        let wire_name = get_wire_name !description name in
        let call = Rpc.call wire_name [(Rpc.Dict cur)] in
        let r = rpc call in
        if r.Rpc.success
        then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> Ok x | Error (`Msg x) -> raise (MarshalError x)
        else match Rpcmarshal.unmarshal e.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> Error x | Error (`Msg x) -> raise  (MarshalError x)
    in inner [] ty
end

module GenClientExn () =
struct
  type implementation = client_implementation
  let description = ref None
  let implement x = description := Some x; ()

  type ('a,'b) comp = 'a
  type rpcfn = Rpc.call -> Rpc.response
  type 'a res = rpcfn -> 'a

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Error.t) -> ('a,_) comp fn

  let returning a err = Returning (a, err)
  let (@->) = fun t f -> Function (t, f)

  let declare name _ ty (rpc : rpcfn) =
    let open Result in
    let rec inner : type b. (string * Rpc.t) list -> b fn -> b = fun cur ->
      function
      | Function (t, f) ->
        fun v -> inner ((t.Param.name, Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v) :: cur) f
      | Returning (t, e) ->
        let wire_name = get_wire_name !description name in
        let call = Rpc.call wire_name [(Rpc.Dict cur)] in
        let r = rpc call in
        if r.Rpc.success
        then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> x | Error (`Msg x) -> raise (MarshalError x)
        else match Rpcmarshal.unmarshal e.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> raise (e.Error.raiser x) | Error (`Msg x) -> raise (MarshalError x)
    in inner [] ty
end

(* For the Server generation, the 'implement' function call _must_ be called
   before any RPCs are described. This exception will be raised if the user
   tries to do this. *)
exception NoDescription

module GenServer () = struct
  open Rpc

  let funcs = Hashtbl.create 20

  let server call =
    let fn = try Hashtbl.find funcs call.name with Not_found -> raise (UnknownMethod call.name) in
    fn call

  type implementation = Rpc.call -> Rpc.response
  let description = ref None
  let implement x = description := Some x; server

  type ('a,'b) comp = ('a,'b) Result.result
  type rpcfn = Rpc.call -> Rpc.response
  type funcs = (string, rpcfn) Hashtbl.t
  type 'a res = 'a -> unit

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Error.t) -> ('a, 'b) comp fn

  let returning a b = Returning (a,b)
  let (@->) = fun t f -> Function (t, f)

  let declare : string -> string list -> 'a fn -> 'a res = fun name _ ty impl ->
    begin
      (* Sanity check: ensure the description has been set before we declare
         any RPCs *)
      match !description with
      | Some _ -> ()
      | None -> raise NoDescription
    end;
    let open Rresult.R in
    let rpcfn =
      let rec inner : type a. a fn -> a -> call -> response = fun f impl call ->
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
          match impl with
          | Result.Ok x -> success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty x)
          | Result.Error y -> failure (Rpcmarshal.marshal e.Error.def.Rpc.Types.ty y)
          end
      in inner ty impl
    in

    let wire_name = get_wire_name !description name in
    Hashtbl.add funcs wire_name rpcfn
end

module GenServerExn () = struct
  open Rpc

  let funcs = Hashtbl.create 20

  let server call =
    let fn = try Hashtbl.find funcs call.name with Not_found -> raise (UnknownMethod call.name) in
    fn call

  type implementation = Rpc.call -> Rpc.response
  let description = ref None
  let implement x = description := Some x; server

  type ('a,'b) comp = 'a
  type rpcfn = Rpc.call -> Rpc.response
  type funcs = (string, rpcfn) Hashtbl.t
  type 'a res = 'a -> unit

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Error.t) -> ('a, _) comp fn

  let returning a b = Returning (a,b)
  let (@->) = fun t f -> Function (t, f)

  type boxed_error = BoxedError : 'a Error.t -> boxed_error

  let rec get_error_ty : type a. a fn -> boxed_error = function
    | Function (_,f) -> get_error_ty f
    | Returning (_,e) -> BoxedError e

  let declare : string -> string list -> 'a fn -> 'a res = fun name _ ty impl ->
    let open Rresult.R in
    let rpcfn =
      let rec inner : type a. a fn -> a -> call -> response = fun f impl call ->
        let args = get_named_args call in
        try
          match f with
          | Function (t, f) ->
              let arg_rpc = get_arg args t.Param.name in
              let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
              let arg =
                match z with
                | Result.Ok arg -> arg
                | Result.Error (`Msg m) -> raise (MarshalError m)
              in
              inner f (impl arg) call
          | Returning (t,e) -> success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty impl)
        with e ->
          let BoxedError error_ty = get_error_ty f in
          match error_ty.Error.matcher e with
          | Some y -> failure (Rpcmarshal.marshal error_ty.Error.def.Rpc.Types.ty y)
          | None -> raise e
      in inner ty impl
    in

    let wire_name = get_wire_name !description name in
    Hashtbl.add funcs wire_name rpcfn

end


(* A default error variant as an example. In real code, this is more easily expressed by using the PPX:

       type default_error = InternalError of string [@@deriving rpcty]
z*)

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
