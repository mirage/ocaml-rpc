(* Basic type definitions *)
open Rpc.Types

type err = [`Msg of string]

type marshalty = Complete | Partial of (string * marshalty) list

let tailrec_map f l = List.rev_map f l |> List.rev

let rec unmarshal : type a. a typ -> Rpc.t -> (a, err) Result.result =
 fun t v ->
  let open Result in
  let open Rresult.R in
  let open Rpc.ResultUnmarshallers in
  let list_helper typ l =
    List.fold_left
      (fun acc v ->
        match (acc, unmarshal typ v) with
        | Ok a, Ok v -> Ok (v :: a)
        | _, Error (`Msg s) ->
            Error
              (Rresult.R.msg
                 (Printf.sprintf
                    "Failed to unmarshal array: %s (when unmarshalling: %s)" s
                    (Rpc.to_string v)))
        | x, _ -> x )
      (Ok []) l
    >>| List.rev
  in
  match t with
  | Basic Int -> int_of_rpc v
  | Basic Int32 -> int32_of_rpc v
  | Basic Int64 -> int64_of_rpc v
  | Basic Bool -> bool_of_rpc v
  | Basic Float -> float_of_rpc v
  | Basic String -> string_of_rpc v
  | Basic Char -> int_of_rpc v >>| Char.chr
  | DateTime -> dateTime_of_rpc v
  | Array typ -> (
    match v with
    | Enum xs -> list_helper typ xs >>| Array.of_list
    | _ -> Rresult.R.error_msg "Expecting Array" )
  | List (Tuple (Basic String, typ)) -> (
    match v with
    | Dict xs ->
        let keys = tailrec_map fst xs in
        let vs = tailrec_map snd xs in
        list_helper typ vs >>= fun vs -> return (List.combine keys vs)
    | _ -> Rresult.R.error_msg "Unhandled" )
  | Dict (basic, typ) -> (
    match v with
    | Dict xs -> (
      match basic with
      | String ->
          let keys = tailrec_map fst xs in
          let vs = tailrec_map snd xs in
          list_helper typ vs >>= fun vs -> return (List.combine keys vs)
      | _ -> Rresult.R.error_msg "Expecting something other than a Dict type" )
    | _ -> Rresult.R.error_msg "Unhandled" )
  | List typ -> (
    match v with
    | Enum xs -> list_helper typ xs
    | _ -> Rresult.R.error_msg "Expecting array" )
  | Unit -> unit_of_rpc v
  | Option t -> (
    match v with
    | Enum [x] -> unmarshal t x >>= fun x -> return (Some x)
    | Enum [] -> return None
    | y ->
        Rresult.R.error_msg
          (Printf.sprintf "Expecting an Enum value, got '%s'" (Rpc.to_string y))
    )
  | Tuple (t1, t2) -> (
    match (v, t2) with
    | Rpc.Enum list, Tuple (_, _) ->
        unmarshal t1 (List.hd list)
        >>= fun v1 ->
        unmarshal t2 (Rpc.Enum (List.tl list)) >>= fun v2 -> Ok (v1, v2)
    | Rpc.Enum [x; y], _ ->
        unmarshal t1 x >>= fun v1 -> unmarshal t2 y >>= fun v2 -> Ok (v1, v2)
    | Rpc.Enum _, _ -> Rresult.R.error_msg "Too many items in a tuple!"
    | _, _ -> error_msg "Expecting Rpc.Enum when unmarshalling a tuple" )
  | Tuple3 (t1, t2, t3) -> (
    match v with
    | Rpc.Enum [x; y; z] ->
        unmarshal t1 x
        >>= fun v1 ->
        unmarshal t2 y
        >>= fun v2 -> unmarshal t3 z >>= fun v3 -> Ok (v1, v2, v3)
    | Rpc.Enum _ ->
        Rresult.R.error_msg
          "Expecting precisely 3 items when unmarshalling a Tuple3"
    | _ -> error_msg "Expecting Rpc.Enum when unmarshalling a tuple3" )
  | Tuple4 (t1, t2, t3, t4) -> (
    match v with
    | Rpc.Enum [x; y; z; a] ->
        unmarshal t1 x
        >>= fun v1 ->
        unmarshal t2 y
        >>= fun v2 ->
        unmarshal t3 z
        >>= fun v3 -> unmarshal t4 a >>= fun v4 -> Ok (v1, v2, v3, v4)
    | Rpc.Enum _ ->
        Rresult.R.error_msg
          "Expecting precisely 4 items in an Enum when unmarshalling a Tuple4"
    | _ -> error_msg "Expecting Rpc.Enum when unmarshalling a tuple4" )
  | Struct {constructor; sname; _} -> (
    match v with
    | Rpc.Dict keys' ->
        (if List.mem_assoc "__p" keys' then failwith "Found partial structure in unmarshal");
        let keys =
          List.map (fun (s, v) -> (String.lowercase_ascii s, v)) keys'
        in
        constructor
          { field_get=
              (let x : type a.
                   string -> a typ -> (a, Rresult.R.msg) Result.result =
                fun s ty ->
                 let s = String.lowercase_ascii s in
                 match ty with
                 | Option x -> (
                   try
                     List.assoc s keys |> unmarshal x
                     >>= fun o -> return (Some o)
                   with _ -> return None )
                 | y ->
                   try List.assoc s keys |> unmarshal y with Not_found ->
                     error_msg
                       (Printf.sprintf
                          "No value found for key: '%s' when unmarshalling '%s'"
                          s sname)
               in
               x)  }
    | _ ->
        error_msg
          (Printf.sprintf "Expecting Rpc.Dict when unmarshalling a '%s' (got %s)" sname (Rpc.to_string v))
    )
  | Variant {vconstructor; _} ->
      ( match v with
      | Rpc.String name -> ok (name, Rpc.Null)
      | Rpc.Enum [Rpc.String name; contents] -> ok (name, contents)
      | _ -> error_msg "Expecting String or Enum when unmarshalling a variant"
      )
      >>= fun (name, contents) ->
      let constr = {tget= (fun typ -> unmarshal typ contents)} in
      vconstructor name constr
  | Abstract {of_rpc; _} -> of_rpc v
  | Refv (cls,typ) -> begin
    match v with
    | Rpc.String x -> Ok (make_ref cls typ x)
    | _ -> error_msg (Printf.sprintf "Expecting Rpc.String when unmarshalling a reference of typ: %s" (Rpc.Types.string_of_typ typ))
    end
  | Refmap ty -> begin
    match v with
    | Rpc.Dict xs ->
      if List.mem_assoc "__p" xs then failwith "Not expecting partial!";
      List.fold_left (fun refmap (ref,v) -> refmap >>= fun r -> unmarshal (Option ty) v >>= function | Some record -> Ok (Refmap.add ref record r) | None -> Error (`Msg ("Expecting non-optional values when unmarshalling a complete refmap")) )
        (Ok Refmap.empty) xs
    | _ -> error_msg "Expecting Rpc.Dict when unmarshalling a refmap"
    end  

let rec unmarshal_partial : type a. a typ -> a -> Rpc.t -> (a, err) Result.result = fun t cur v ->
  let open Rresult.R in
  match t,v with
  | Struct { fields; _ }, Dict d ->
    if not (List.mem_assoc "__p" d) then unmarshal t v else
    List.fold_left (fun acc f -> match f with | BoxedField fld ->
      match fld.fname with
      | [fname] -> acc >>= fun a -> (try unmarshal_partial fld.field (fld.fget a) (List.assoc fname d) >>= fun x -> Ok (fld.fset x a) with _ -> Ok a)
      | _ -> Rresult.R.error_msg (Printf.sprintf "Expecting single path fields in unmarshal_partial (got %s)" (String.concat "," fld.fname))
    ) (Ok cur) fields
  | Refmap typ, Dict d ->
    let is_partial, others =
      let rec inner = function
      | [] -> false, []
      | (x,_)::xs when x="__p" ->
        (true, xs)
      | x::xs -> let (res,others) = inner xs in (res,x::others)
      in inner d 
    in
    if not is_partial
    then (unmarshal t v)
    else
    List.fold_left (fun acc (r,rpc) ->
      acc >>= fun a ->
      (match rpc with
      | Rpc.Enum [x] -> 
        let obj =
          try
            unmarshal_partial typ (Refmap.find r a) x
          with _ ->
            unmarshal typ x
        in 
        obj >>= fun x -> Ok (Some x)
      | Rpc.Enum [] ->
        Ok None
      | x ->
        Error (`Msg (Printf.sprintf "Expecting enum values when unmarshalling a Refmap (got %s)" (Rpc.to_string x))))
      >>= function Some x -> Ok (Refmap.add r x a) | None -> Ok (Refmap.remove r a)
      ) (Ok cur) others
  | _ -> unmarshal t v

let rec marshal_partial : type a. a typ -> marshalty -> a -> Rpc.t =
 fun ty marshalty v ->
  let open Rpc in
  let rpc_of_basic : type a. a basic -> a -> Rpc.t =
   fun t v ->
    match t with
    | Int -> rpc_of_int v
    | Int32 -> rpc_of_int32 v
    | Int64 -> rpc_of_int64 v
    | Bool -> rpc_of_bool v
    | Float -> rpc_of_float v
    | String -> rpc_of_string v
    | Char -> rpc_of_int (Char.code v)
  in
  match ty, marshalty with
  | Basic t, Complete -> rpc_of_basic t v
  | DateTime, Complete -> rpc_of_dateTime v
  | Array typ, Complete -> Enum (tailrec_map (marshal_partial typ Complete) (Array.to_list v))
  | List (Tuple (Basic String, typ)), Complete ->
      Dict (tailrec_map (fun (x, y) -> (x, marshal_partial typ Complete y)) v)
  | List typ, Complete -> Enum (tailrec_map (marshal_partial typ Complete) v)
  | Dict (String, typ), Complete ->
      Rpc.Dict (tailrec_map (fun (k, v) -> (k, marshal_partial typ Complete v)) v)
  | Dict (basic, typ), Complete ->
      Rpc.Enum
        (tailrec_map
           (fun (k, v) -> Rpc.Enum [rpc_of_basic basic k; marshal_partial typ Complete v])
           v)
  | Unit, Complete -> rpc_of_unit v
  | Option ty, Complete -> Rpc.Enum (match v with Some x -> [marshal_partial ty Complete x] | None -> [])
  | Tuple (x, (Tuple (_, _) as y)), Complete -> (
    match marshal_partial y Complete (snd v) with
    | Rpc.Enum xs -> Rpc.Enum (marshal_partial x Complete (fst v) :: xs)
    | _ -> failwith "Marshalling a tuple should always give an Enum" )
  | Tuple (x, y), Complete -> Rpc.Enum [marshal_partial x Complete (fst v); marshal_partial y Complete (snd v)]
  | Tuple3 (x, y, z), Complete ->
      let vx, vy, vz = v in
      Rpc.Enum [marshal_partial x Complete vx; marshal_partial y Complete vy; marshal_partial z Complete vz]
  | Tuple4 (x, y, z, a), Complete ->
      let vx, vy, vz, va = v in
      Rpc.Enum [marshal_partial x Complete vx; marshal_partial y Complete vy; marshal_partial z Complete vz; marshal_partial a Complete va]
  | Struct {fields; _}, Complete ->
      let fields =
        List.fold_left
          (fun acc f ->
            match f with BoxedField f ->
              let key = String.concat "." f.fname in
              let value = marshal_partial f.field Complete (f.fget v) in
              match (f.field, value) with
              | Option _, Rpc.Enum [] -> acc
              | Option _, Rpc.Enum [x] -> (key, x) :: acc
              | _, _ -> (key,value)::acc )
          [] fields
      in
      Rpc.Dict fields
  | Struct {fields; _}, Partial keys ->
      let fields =
        List.fold_left
          (fun acc f ->
            match f with BoxedField f ->
              let key = String.concat "." f.fname in
              match List.assoc_opt key keys with
              | Some c -> (key, marshal_partial f.field c (f.fget v)) :: acc
              | None -> acc)
          [] fields
      in
      Rpc.Dict (("__p",Rpc.Bool true) :: fields)
  | Variant {variants; _}, Complete ->
      List.fold_left
        (fun acc t ->
          match t with BoxedTag t ->
            match t.tpreview v with
            | Some x -> (
              match marshal_partial t.tcontents Complete x with
              | Rpc.Null -> Rpc.String t.tname
              | y -> Rpc.Enum [Rpc.String t.tname; y] )
            | None -> acc )
        Rpc.Null variants
  | Abstract {rpc_of; _}, Complete -> rpc_of v
  | Refv (_cls,_typ), Complete ->
      rpc_of_string (name_of_ref v)
  | Refmap typ, Complete -> begin
      let keys = Refmap.keys v |> List.sort String.compare in
      Rpc.Dict (List.map (fun key -> (key, marshal_partial (Option typ) Complete (Some (Refmap.find key v)))) keys)
    end
  | Refmap typ, Partial keys -> begin
      Rpc.Dict (("__p",Rpc.Bool true) :: (List.map (fun (key,subkeys) ->
        let v =
          if Refmap.mem key v
          then Rpc.Enum [marshal_partial typ subkeys (Refmap.find key v)]
          else Rpc.Enum []
        in (key,v)) keys))
      end
  | typ, Partial _ ->
    failwith (Printf.sprintf "Can't marshal_partial a typ %s" (Rpc.Types.string_of_typ typ))

let marshal : type a. a typ -> a -> Rpc.t = fun ty v -> marshal_partial ty Complete v
 
let ocaml_of_t = Rpc.Types.string_of_typ