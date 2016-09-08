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
  | Struct { constructor; sname } -> begin
    match v with
    | Rpc.Dict keys ->
      constructor { g = (fun s ty ->
          try
            List.assoc s keys |> unmarshal ty
          with Not_found ->
            error_of_string
              (Printf.sprintf
                 "No value found for key: '%s' when unmarshalling '%s'" s sname)
        ) }
    | _ ->
      error_of_string (Printf.sprintf "Expecting Rpc.Dict when unmarshalling a '%s'" sname)
    end
  | Variant { vconstructor; variants } ->
    (match v with
      | Rpc.String name -> Monad.return (name, Rpc.Null)
      | Rpc.Enum [ Rpc.String name; contents ] -> Monad.return (name, contents)
      | _ -> error_of_string "Expecting String or Enum when unmarshalling a variant")
    >>= fun (name, contents) ->
    let constr = { t = fun typ -> unmarshal typ contents } in
    vconstructor name constr


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
  | Struct { fields } -> begin
      let fields = List.map (function
          | BoxedField f ->
            (f.fname, marshal f.field (f.fget v))) fields
      in Rpc.Dict fields
    end
  | Variant { variants } -> begin
      List.fold_left (fun acc t ->
          match t with
          | BoxedTag t ->
            match t.vpreview v with
            | Some x -> begin
                match marshal t.vcontents x with
                | Rpc.Null ->
                  Rpc.String t.vname
                | y ->
                  Rpc.Enum [ Rpc.String t.vname; y ]
              end
            | None -> acc) Rpc.Null variants
    end


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
