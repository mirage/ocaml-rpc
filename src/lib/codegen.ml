open Rpc.Types

type _ outerfn =
  | Function : 'a Idl.Param.t * 'b outerfn -> ('a -> 'b) outerfn
  | NoArgsFunction : 'b outerfn -> (unit -> 'b) outerfn
  | Returning : ('a Idl.Param.t * 'b Idl.Error.t) -> ('a, 'b) Result.t outerfn

module Method = struct
  type 'a t =
    { name : string
    ; description : string list
    ; ty : 'a outerfn
    ; is_notification : bool
    }

  let rec find_inputs : type a. a outerfn -> Idl.Param.boxed list =
   fun m ->
    match m with
    | Function (x, y) -> Idl.Param.Boxed x :: find_inputs y
    | NoArgsFunction y -> find_inputs y
    | Returning _ -> []


  let rec find_output : type a. a outerfn -> Idl.Param.boxed =
   fun m ->
    match m with
    | Returning (x, _y) -> Idl.Param.Boxed x
    | NoArgsFunction y -> find_output y
    | Function (_x, y) -> find_output y


  let rec find_errors : type a. a outerfn -> Rpc.Types.boxed_def =
   fun m ->
    match m with
    | Returning (_x, y) -> Rpc.Types.BoxedDef y.Idl.Error.def
    | NoArgsFunction y -> find_errors y
    | Function (_x, y) -> find_errors y
end

type boxed_fn = BoxedFunction : 'a Method.t -> boxed_fn

module Interface = struct
  include Idl.Interface

  type t =
    { details : Idl.Interface.description
    ; methods : boxed_fn list
    }

  let prepend_arg : t -> 'a Idl.Param.t -> t =
   fun interface param ->
    let prepend : type b. b outerfn -> ('a -> b) outerfn =
     fun arg -> Function (param, arg)
    in
    { interface with
      methods =
        List.map
          (fun (BoxedFunction m) ->
            BoxedFunction
              Method.
                { name = m.name
                ; description = m.description
                ; ty = prepend m.ty
                ; is_notification = m.is_notification
                })
          interface.methods
    }


  let setify l =
    List.fold_left (fun set x -> if List.mem x set then set else x :: set) [] l
    |> List.rev


  let all_types : t -> boxed_def list =
   fun i ->
    let all_inputs =
      List.map
        (function
          | BoxedFunction f -> Method.(find_inputs f.ty))
        i.methods
    in
    let all_outputs =
      List.map
        (function
          | BoxedFunction f -> Method.(find_output f.ty))
        i.methods
    in
    let all = List.concat (all_inputs @ [ all_outputs ]) in
    let types = List.map (fun (Idl.Param.Boxed p) -> BoxedDef p.Idl.Param.typedef) all in
    setify types


  let all_errors i =
    i.methods
    |> List.map (function BoxedFunction f -> Method.(find_errors f.ty))
    |> setify
end

module Interfaces = struct
  type t =
    { name : string
    ; title : string
    ; description : string list
    ; type_decls : boxed_def list
    ; error_decls : boxed_def list
    ; interfaces : Interface.t list
    }

  let empty name title description =
    { name; title; description; type_decls = []; error_decls = []; interfaces = [] }


  let add_interface i is =
    let not_in defs (BoxedDef def) =
      not (List.exists (fun (BoxedDef def') -> def'.name = def.name) defs)
    in
    let typedefs = Interface.all_types i in
    let new_typedefs = List.filter (not_in is.type_decls) typedefs in
    let new_errors = List.filter (not_in is.error_decls) (Interface.all_errors i) in
    { is with
      type_decls = new_typedefs @ is.type_decls
    ; error_decls = new_errors @ is.error_decls
    ; interfaces = i :: is.interfaces
    }


  let create ~name ~title ~description ~interfaces =
    let i = empty name title description in
    List.fold_right add_interface interfaces i
end

exception Interface_not_described

module Gen () = struct
  type ('a, 'b) comp = ('a, 'b) Result.t
  type 'a fn = 'a outerfn
  type 'a res = unit
  type implementation = unit -> Interface.t

  let methods = ref []

  let implement i () =
    let n = i.Interface.name in
    if String.capitalize_ascii n <> n then failwith "Interface names must be capitalized";
    let i = Interface.{ details = i; methods = List.rev !methods } in
    i


  let returning a b = Returning (a, b)
  let ( @-> ) t f = Function (t, f)
  let noargs f = NoArgsFunction f

  let declare_ is_notification name description ty =
    let m = BoxedFunction Method.{ name; description; ty; is_notification } in
    methods := m :: !methods


  let declare : string -> string list -> 'a fn -> 'a res =
   fun name description ty -> declare_ false name description ty


  let declare_notification name description ty = declare_ true name description ty
end
