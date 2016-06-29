(* Basic type definitions *)
open Rpc.Types
       

let rec unmarshal : type a. a typ -> Rpc.t -> a Rpc.Monad.error_or = fun t v ->
  let open Rpc in
  let open Monad in
  let open Result in
  let list_helper typ l =
    List.fold_left (fun acc v ->
        match acc, unmarshal typ v with
        | (Ok a), (Ok v) -> Ok (v::a)
        | _ -> error_of_string "Failed to unmarshal array")
      (Ok []) l
  in
  match t with
  | Basic Int -> int_of_rpc v
  | Basic Int32 -> int32_of_rpc v
  | Basic Int64 -> int64_of_rpc v
  | Basic Bool -> bool_of_rpc v
  | Basic Float -> float_of_rpc v
  | Basic String -> string_of_rpc v
  | Basic Char -> lift Char.chr (int_of_rpc v)
  | DateTime -> dateTime_of_rpc v
  | Array typ -> begin
      match v with
      | Enum xs -> lift Array.of_list (list_helper typ xs)
      | _ -> error_of_string "Expecting Array"
    end
  | List typ -> begin
      match v with
      | Enum xs -> list_helper typ xs
      | _ -> error_of_string "Expecting array"
    end
  | Dict (basic, typ) -> begin
      match v with
      | Dict xs -> begin
        match basic with
        | String ->
          let keys = List.map fst xs in
          let vs = List.map snd xs in
          list_helper typ vs >>= fun vs ->
          return (List.combine keys vs)
        | _ -> 
          error_of_string "Expecting something other than a Dict type"
        end
      | _ ->
        error_of_string "Unhandled"
    end
  | Unit -> unit_of_rpc v
  | Option _ ->
    error_of_string "Unhandled"
  | Tuple (t1, t2) -> begin
      match v, t2 with
      | Rpc.Enum list, Tuple (_, _) ->
        unmarshal t1 (List.hd list) >>= fun v1 ->
        unmarshal t2 (Rpc.Enum (List.tl list)) >>= fun v2 ->
        Ok (v1, v2)
      | Rpc.Enum ([x;y]), _ ->
        unmarshal t1 x >>= fun v1 ->
        unmarshal t2 y >>= fun v2 ->
        Ok (v1,v2)
      | Rpc.Enum list, _ ->
        error_of_string "Too many items in a tuple!"
      | _, _ ->
        error_of_string "Expecting Rpc.Enum when unmarshalling a tuple"      
    end
  | Struct {sname; fields} -> begin
      match v with
      | Dict dict ->
        (* Check each field is OK *)
        List.fold_left (fun acc field ->
            acc >>= fun acclist ->
            let BoxedField f = field in
            let k = f.fname in
            if List.mem_assoc k dict
            then
              let v = List.assoc f.fname dict in
              (unmarshal f.field v >>= fun _ -> Ok ((k, v)::acclist))
            else
              error_of_string (Printf.sprintf "Field '%s' not found when unmarshalling structure" f.fname))
          (Ok [])
          fields >>= fun vfields ->
        Ok { vfields }
      | _ -> 
        error_of_string "Unhandled"
    end
  | Variant { variants } -> begin
      match v with
      | Rpc.String x ->
        if not (List.exists (fun (BoxedTag t) -> t.vname = x) variants)
        then error_of_string (Printf.sprintf "Unknown variant: %s" x)
        else begin
          let BoxedTag tag = List.find (fun (BoxedTag t) -> t.vname = x) variants in
          match tag.vcontents with
          | Unit -> Ok ({ tag=tag.vname; contents=Rpc.Null }) 
          | _ -> error_of_string (Printf.sprintf "Tag '%s' expects contents, none received" x)
        end
      | Rpc.Enum ((Rpc.String x)::xs) -> begin
          if not (List.exists (fun (BoxedTag t) -> t.vname = x) variants)
          then error_of_string (Printf.sprintf "Unknown variant: %s" x)
          else
            let BoxedTag tag = List.find (fun (BoxedTag t) -> t.vname = x) variants in
            unmarshal tag.vcontents (Rpc.Enum xs) >>= fun _ ->
            Ok { tag=x; contents=Rpc.Enum xs }
        end
      | _ -> error_of_string "Expecting a string or Enum"
    end
    
let rec marshal : type a. a typ -> a -> Rpc.t = fun t v ->
  let open Rpc in
  let open Monad in
  let open Result in
  let rpc_of_basic : type a. a basic -> a -> Rpc.t = fun t v ->
    match t with
    | Int -> rpc_of_int v
    | Int32 -> rpc_of_int32 v
    | Int64 -> rpc_of_int64 v
    | Bool -> rpc_of_bool v
    | Float -> rpc_of_float v
    | String -> rpc_of_string v
    | Char -> rpc_of_int (Char.code v)
  in
  match t with
  | Basic t -> rpc_of_basic t v
  | DateTime -> rpc_of_dateTime v
  | Array typ -> Enum (List.map (marshal typ) (Array.to_list v))
  | List typ -> Enum (List.map (marshal typ) v)
  | Dict (String, typ) ->
    Rpc.Dict (List.map (fun (k,v) -> (k, marshal typ v)) v)
  | Dict (basic, typ) ->
    Rpc.Enum (List.map (fun (k,v) -> Rpc.Enum [ rpc_of_basic basic k; marshal typ v ]) v)
  | Unit -> rpc_of_unit v
  | Option _ ->
    failwith "Unhandled"
  | Tuple (x, (Tuple (_,_) as y)) -> begin
      match marshal y (snd v) with
      | Rpc.Enum xs -> Rpc.Enum ((marshal x (fst v))::xs)
      | _ -> failwith "Marshalling a tuple should always give an Enum"
    end
  | Tuple (x, y) ->
    Rpc.Enum [marshal x (fst v); marshal y (snd v)]
  | Struct {sname; fields} -> begin
      let fields = List.map (function          
          | BoxedField f ->
            (f.fname, List.assoc f.fname v.vfields)) fields
      in Rpc.Dict fields
    end
  | Variant _ -> begin
      let { tag; contents } = v in
      Rpc.Enum [ Rpc.String tag; contents ]
    end
    
let getf : type t a. (a, t) field -> t structure_value -> a Rpc.Monad.error_or =
  fun field v ->
    let v = List.assoc field.fname v.vfields in
    unmarshal field.field v

let setf : type t a. (a, t) field -> t structure_value -> a -> t structure_value =
  fun field str v ->
    { vfields = (field.fname, marshal field.field v) :: (List.filter (fun (name, _) -> name <> field.fname) str.vfields) }

let mkvar : type t a. (a, t) tag -> a -> t variant_value =
  fun variant contents -> 
  { tag = variant.vname; contents = marshal variant.vcontents contents }


let ocaml_of_basic : type a. a basic -> string = function
  | Int64 -> "int64"
  | Int32 -> "int32"
  | Int -> "int"
  | String -> "string"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"

let rec ocaml_of_t : type a. a typ -> string = function
  | Basic b -> ocaml_of_basic b
  | DateTime -> "string"
  | Array t -> ocaml_of_t t ^ " list"
  | List t -> ocaml_of_t t ^ " list"
  | Dict (b,t) -> Printf.sprintf "(%s * %s) list" (ocaml_of_basic b) (ocaml_of_t t)
  | Unit -> "unit"
  | Option t -> ocaml_of_t t ^ " option"
  | Tuple (a,b) -> Printf.sprintf "(%s * %s)" (ocaml_of_t a) (ocaml_of_t b)
  | Struct { sname; fields } ->
    let fields = List.map (function
        | BoxedField f ->
          Printf.sprintf "%s: %s;" f.fname (ocaml_of_t f.field)) fields in
    Printf.sprintf "{ %s }" (String.concat " " fields)            
  | Variant { variants } ->
    let tags = List.map (function
        | BoxedTag t ->
          Printf.sprintf "| %s (%s) (** %s *)" t.vname (ocaml_of_t t.vcontents) t.vdescription) variants in
    String.concat " " tags




