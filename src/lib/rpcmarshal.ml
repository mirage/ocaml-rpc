(* Basic type definitions *)
open Rpc.Types

type err = [ `Msg of string ]

let tailrec_map f l = List.rev_map f l |> List.rev

let rec unmarshal : type a. a typ -> Rpc.t -> (a, err) Result.t =
 fun t v ->
  let open Rpc in
  let open Result in
  let open Rresult.R in
  let open Rpc.ResultUnmarshallers in
  let list_helper typ l =
    List.fold_left
      (fun acc v ->
        match acc, unmarshal typ v with
        | Ok a, Ok v -> Ok (v :: a)
        | _, Error (`Msg s) ->
          Error
            (Rresult.R.msg
               (Printf.sprintf
                  "Failed to unmarshal array: %s (when unmarshalling: %s)"
                  s
                  (Rpc.to_string v)))
        | x, _ -> x)
      (Ok [])
      l
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
  | Base64 -> base64_of_rpc v
  | Array typ ->
    (match v with
    | Enum xs -> list_helper typ xs >>| Array.of_list
    | _ -> Rresult.R.error_msg "Expecting Array")
  | List (Tuple (Basic String, typ)) ->
    (match v with
    | Dict xs ->
      let keys = tailrec_map fst xs in
      let vs = tailrec_map snd xs in
      list_helper typ vs >>= fun vs -> return (List.combine keys vs)
    | _ -> Rresult.R.error_msg "Unhandled")
  | Dict (basic, typ) ->
    (match v with
    | Dict xs ->
      (match basic with
      | String ->
        let keys = tailrec_map fst xs in
        let vs = tailrec_map snd xs in
        list_helper typ vs >>= fun vs -> return (List.combine keys vs)
      | _ -> Rresult.R.error_msg "Expecting something other than a Dict type")
    | _ -> Rresult.R.error_msg "Unhandled")
  | List typ ->
    (match v with
    | Enum xs -> list_helper typ xs
    | _ -> Rresult.R.error_msg "Expecting array")
  | Unit -> unit_of_rpc v
  | Option t ->
    (match v with
    | Enum [ x ] -> unmarshal t x >>= fun x -> return (Some x)
    | Enum [] -> return None
    | y ->
      Rresult.R.error_msg
        (Printf.sprintf "Expecting an Enum value, got '%s'" (Rpc.to_string y)))
  | Tuple (t1, t2) ->
    (match v, t2 with
    | Rpc.Enum list, Tuple (_, _) ->
      unmarshal t1 (List.hd list)
      >>= fun v1 -> unmarshal t2 (Rpc.Enum (List.tl list)) >>= fun v2 -> Ok (v1, v2)
    | Rpc.Enum [ x; y ], _ ->
      unmarshal t1 x >>= fun v1 -> unmarshal t2 y >>= fun v2 -> Ok (v1, v2)
    | Rpc.Enum _, _ -> Rresult.R.error_msg "Too many items in a tuple!"
    | _, _ -> error_msg "Expecting Rpc.Enum when unmarshalling a tuple")
  | Tuple3 (t1, t2, t3) ->
    (match v with
    | Rpc.Enum [ x; y; z ] ->
      unmarshal t1 x
      >>= fun v1 ->
      unmarshal t2 y >>= fun v2 -> unmarshal t3 z >>= fun v3 -> Ok (v1, v2, v3)
    | Rpc.Enum _ ->
      Rresult.R.error_msg "Expecting precisely 3 items when unmarshalling a Tuple3"
    | _ -> error_msg "Expecting Rpc.Enum when unmarshalling a tuple3")
  | Tuple4 (t1, t2, t3, t4) ->
    (match v with
    | Rpc.Enum [ x; y; z; a ] ->
      unmarshal t1 x
      >>= fun v1 ->
      unmarshal t2 y
      >>= fun v2 ->
      unmarshal t3 z >>= fun v3 -> unmarshal t4 a >>= fun v4 -> Ok (v1, v2, v3, v4)
    | Rpc.Enum _ ->
      Rresult.R.error_msg
        "Expecting precisely 4 items in an Enum when unmarshalling a Tuple4"
    | _ -> error_msg "Expecting Rpc.Enum when unmarshalling a tuple4")
  | Struct { constructor; sname; _ } ->
    (match v with
    | Rpc.Dict keys' ->
      let keys = List.map (fun (s, v) -> String.lowercase_ascii s, v) keys' in
      constructor
        { field_get =
            (let x : type a. string -> a typ -> (a, Rresult.R.msg) Result.t =
              fun s ty ->
               let s = String.lowercase_ascii s in
               match ty with
               | Option x ->
                 (try List.assoc s keys |> unmarshal x >>= fun o -> return (Some o) with
                 | _ -> return None)
               | y ->
                 (try List.assoc s keys |> unmarshal y with
                 | Not_found ->
                   error_msg
                     (Printf.sprintf
                        "No value found for key: '%s' when unmarshalling '%s'"
                        s
                        sname))
             in
             x)
        }
    | _ -> error_msg (Printf.sprintf "Expecting Rpc.Dict when unmarshalling a '%s'" sname))
  | Variant { vconstructor; _ } ->
    (match v with
    | Rpc.String name -> ok (name, Rpc.Null)
    | Rpc.Enum [ Rpc.String name; contents ] -> ok (name, contents)
    | _ -> error_msg "Expecting String or Enum when unmarshalling a variant")
    >>= fun (name, contents) ->
    let constr = { tget = (fun typ -> unmarshal typ contents) } in
    vconstructor name constr
  | Abstract { of_rpc; _ } -> of_rpc v


let rec marshal : type a. a typ -> a -> Rpc.t =
 fun t v ->
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
  match t with
  | Basic t -> rpc_of_basic t v
  | DateTime -> rpc_of_dateTime v
  | Base64 -> rpc_of_base64 v
  | Array typ -> Enum (tailrec_map (marshal typ) (Array.to_list v))
  | List (Tuple (Basic String, typ)) ->
    Dict (tailrec_map (fun (x, y) -> x, marshal typ y) v)
  | List typ -> Enum (tailrec_map (marshal typ) v)
  | Dict (String, typ) -> Rpc.Dict (tailrec_map (fun (k, v) -> k, marshal typ v) v)
  | Dict (basic, typ) ->
    Rpc.Enum
      (tailrec_map (fun (k, v) -> Rpc.Enum [ rpc_of_basic basic k; marshal typ v ]) v)
  | Unit -> rpc_of_unit v
  | Option ty ->
    Rpc.Enum
      (match v with
      | Some x -> [ marshal ty x ]
      | None -> [])
  | Tuple (x, (Tuple (_, _) as y)) ->
    (match marshal y (snd v) with
    | Rpc.Enum xs -> Rpc.Enum (marshal x (fst v) :: xs)
    | _ -> failwith "Marshalling a tuple should always give an Enum")
  | Tuple (x, y) -> Rpc.Enum [ marshal x (fst v); marshal y (snd v) ]
  | Tuple3 (x, y, z) ->
    let vx, vy, vz = v in
    Rpc.Enum [ marshal x vx; marshal y vy; marshal z vz ]
  | Tuple4 (x, y, z, a) ->
    let vx, vy, vz, va = v in
    Rpc.Enum [ marshal x vx; marshal y vy; marshal z vz; marshal a va ]
  | Struct { fields; _ } ->
    let fields =
      List.fold_left
        (fun acc f ->
          match f with
          | BoxedField f ->
            let value = marshal f.field (f.fget v) in
            (match f.field, value with
            | Option _, Rpc.Enum [] -> acc
            | Option _, Rpc.Enum [ x ] -> (f.fname, x) :: acc
            | _, _ -> (f.fname, value) :: acc))
        []
        fields
    in
    Rpc.Dict fields
  | Variant { variants; _ } ->
    List.fold_left
      (fun acc t ->
        match t with
        | BoxedTag t ->
          (match t.tpreview v with
          | Some x ->
            (match marshal t.tcontents x with
            | Rpc.Null -> Rpc.String t.tname
            | y -> Rpc.Enum [ Rpc.String t.tname; y ])
          | None -> acc))
      Rpc.Null
      variants
  | Abstract { rpc_of; _ } -> rpc_of v


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
  | Base64 -> "base64"
  | Array t -> ocaml_of_t t ^ " list"
  | List t -> ocaml_of_t t ^ " list"
  | Dict (b, t) -> Printf.sprintf "(%s * %s) list" (ocaml_of_basic b) (ocaml_of_t t)
  | Unit -> "unit"
  | Option t -> ocaml_of_t t ^ " option"
  | Tuple (a, b) -> Printf.sprintf "(%s * %s)" (ocaml_of_t a) (ocaml_of_t b)
  | Tuple3 (a, b, c) ->
    Printf.sprintf "(%s * %s * %s)" (ocaml_of_t a) (ocaml_of_t b) (ocaml_of_t c)
  | Tuple4 (a, b, c, d) ->
    Printf.sprintf
      "(%s * %s * %s * %s)"
      (ocaml_of_t a)
      (ocaml_of_t b)
      (ocaml_of_t c)
      (ocaml_of_t d)
  | Struct { fields; _ } ->
    let fields =
      List.map
        (function
          | BoxedField f -> Printf.sprintf "%s: %s;" f.fname (ocaml_of_t f.field))
        fields
    in
    Printf.sprintf "{ %s }" (String.concat " " fields)
  | Variant { variants; _ } ->
    let tags =
      List.map
        (function
          | BoxedTag t ->
            Printf.sprintf
              "| %s (%s) (** %s *)"
              t.tname
              (ocaml_of_t t.tcontents)
              (String.concat " " t.tdescription))
        variants
    in
    String.concat " " tags
  | Abstract _ -> "<abstract>"
