open Rpc.Types

type 'a comp = 'a
  
type _ fn =
  | Function : 'a Idl.Param.t * 'b fn -> ('a -> 'b) fn
  | Returning : 'a Idl.Param.t -> 'a comp fn
      
module Method = struct
  type 'a t = {
    name : string;
    description : string;
    ty : 'a fn
  }

  let rec find_inputs : type a. a fn -> Idl.Param.boxed list = fun m ->
    match m with
    | Function (x,y) -> (Idl.Param.Boxed x) :: find_inputs y
    | Returning _ -> []
                     
  let rec find_output : type a. a fn -> Idl.Param.boxed = fun m ->
    match m with
    | Returning x -> Idl.Param.Boxed x
    | Function (x,y) -> find_output y
end

type boxed_fn =
  | BoxedFunction : 'a Method.t -> boxed_fn
    
module Interface = struct
  include Idl.Interface

  type t = {
    details : Idl.Interface.description;
    methods : boxed_fn list;
  }
  
  let prepend_arg : t -> 'a Idl.Param.t -> t = fun interface param ->
    let prepend : type b. b fn -> ('a -> b) fn = fun arg ->
      Function (param, arg)
    in
    {interface with methods = List.map (fun (BoxedFunction m) ->
         BoxedFunction Method.({ name = m.name; description = m.description; ty = prepend m.ty}))
         interface.methods}

  let rec all_types : t -> boxed_def list = fun i ->
    let all_inputs = List.map (function BoxedFunction f -> Method.(find_inputs f.ty)) i.methods in
    let all_outputs = List.map (function BoxedFunction f -> Method.(find_output f.ty)) i.methods in
    let all = List.concat (all_inputs @ [all_outputs]) in
    let types = List.map (fun (Idl.Param.Boxed p) -> BoxedDef p.Idl.Param.typedef) all in
    let rec setify = function
      | [] -> []
      | (x::xs) -> if List.mem x xs then setify xs else x::(setify xs)
    in setify types
end

type description = Interface.t

let describe i =
  let n = i.Interface.name in
  if String.capitalize n <> n then failwith "Interface names must be capitalized";
  Interface.({details=i; methods=[]})

  
module Interfaces = struct
  type t = {
    name : string;
    title : string;
    description : string;
    type_decls : boxed_def list;
    interfaces : Interface.t list;
    exn_decls : boxed_def;
  }  
  
  let empty name title description =
    { name; title; description; exn_decls=BoxedDef int64; type_decls=[]; interfaces=[] }
    
  let register_exn is es =
    { is with exn_decls=BoxedDef es }
    
  let add_interface is i =
    let typedefs = Interface.all_types i in
    let new_typedefs = List.filter
        (fun def -> not
            (List.exists
               (fun (BoxedDef def') ->
                  match def with
                  | BoxedDef d -> def'.name = d.name) is.type_decls)) typedefs in
    
    { is with type_decls = new_typedefs @ is.type_decls; interfaces = i :: is.interfaces }
    
end

type 'a res = Interface.t -> Interface.t
                               
let returning a = Returning a
let (@->) = fun t f -> Function (t, f)
        
let declare name description ty interface =
  let m = BoxedFunction Method.({name; description; ty}) in
  Interface.({interface with methods = interface.methods @ [m]})

