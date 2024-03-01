(* Generate typed values *)

open Rpc.Types

type err = [ `Msg of string ]

let badstuff msg = failwith (Printf.sprintf "Failed to construct the record: %s" msg)

module SeenType = struct
  type t = T : _ typ -> t
  let compare a b = if a == b then 0 else Stdlib.compare a b
end

module Seen = Set.Make(SeenType)

(* don't use this on recursive types! *)

let rec gentest : type a. Seen.t -> a typ -> a Seq.t =
 fun seen t ->
  let seen_t = SeenType.T t in
  if Seen.mem seen_t seen then Seq.empty
  else
  let gentest t = gentest (Seen.add seen_t seen) t in
  match t with
  | Basic Int -> [ 0; 1; max_int; -1; 1000000 ] |> List.to_seq
  | Basic Int32 -> [ 0l; 1l; Int32.max_int; -1l; 999999l ] |> List.to_seq
  | Basic Int64 -> [ 0L; 1L; Int64.max_int; -1L; 999999999999L ] |> List.to_seq
  | Basic Bool -> [ true; false ] |> List.to_seq
  | Basic Float -> [ 0.0; max_float; min_float; -1.0 ] |> List.to_seq
  | Basic String ->
    [ "Test string"
    ; ""
    ; "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ \
       ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
    ; "\000foo"
    ] |> List.to_seq
  | Basic Char -> [ '\000'; 'a'; 'z'; '\255' ] |> List.to_seq
  | DateTime -> [ "19700101T00:00:00Z" ] |> List.to_seq
  | Base64 -> [ "SGVsbG8sIHdvcmxkIQ==" (* "Hello, world!" *) ] |> List.to_seq
  | Array typ -> [ gentest typ |> Array.of_seq; [||] ] |> List.to_seq
  | List typ -> [ gentest typ |> List.of_seq; [] ] |> List.to_seq
  | Dict (basic, typ) ->
    let keys = gentest (Basic basic) in
    let vs = Seq.cycle (gentest typ) in
    let x = Seq.map2 (fun k v -> k, v) keys vs |> List.of_seq in
    Seq.return x
  | Unit -> Seq.return ()
  | Option t ->
    let vs = gentest t in
    Seq.(append (return None) @@ map (fun x -> Some x) vs)
  | Tuple (t1, t2) ->
    let v1s = gentest t1 in
    let v2s = gentest t2 in
    Seq.product v1s v2s
  | Tuple3 (t1, t2, t3) ->
    let v1s = gentest t1 in
    let v2s = gentest t2 in
    let v3s = gentest t3 in
    Seq.(product (product v1s v2s) v3s |> map (fun ((x,y),z) -> x,y,z))
  | Tuple4 (t1, t2, t3, t4) ->
    let v1s = gentest t1 in
    let v2s = gentest t2 in
    let v3s = gentest t3 in
    let v4s = gentest t4 in
    Seq.(product (product v1s v2s) (product v3s v4s) |> map (fun ((x,y),(z,t)) -> x,y,z,t))
  | Struct { constructor; _ } ->
    let gen _ =
        let field_get : type a. string -> a typ -> (a, Rresult.R.msg) Result.t =
         fun _ ty ->
          let vs = gentest ty |> Array.of_seq in
          Result.Ok (vs.(Random.int (Array.length vs)))
        in
        (match constructor { field_get } with
        | Result.Ok x -> x
        | Result.Error (`Msg y) -> badstuff y)
    in
    Seq.ints 0 |> Seq.take 10 |> Seq.map gen
  | Variant { variants; _ } ->
    variants |> List.to_seq |> Seq.map
      (function
        | Rpc.Types.BoxedTag v ->
          let contents = gentest v.tcontents |> Array.of_seq in
          let content = contents.(Random.int (Array.length contents)) in
          v.treview content)
  | Abstract { test_data; _ } -> test_data |> List.to_seq

let rec genall: type a. maxcomb:int -> Seen.t -> int -> string -> a typ -> a Seq.t =
 fun ~maxcomb seen depth strhint t ->
  let thin d result =
    if d < 0 then Seq.take 1 result else Seq.take maxcomb result
  in
  let seen_t = SeenType.T t in
  if Seen.mem seen_t seen then Seq.empty
  else
  let genall depth strhint t = genall ~maxcomb (Seen.add seen_t seen) depth strhint t in
  match t with
  | Basic Int -> Seq.return 0
  | Basic Int32 -> Seq.return 0l
  | Basic Int64 -> Seq.return 0L
  | Basic Bool -> thin depth (List.to_seq [ true; false ])
  | Basic Float -> Seq.return 0.0
  | Basic String -> Seq.return strhint
  | Basic Char -> Seq.return 'a'
  | DateTime -> Seq.return "19700101T00:00:00Z"
  | Base64 -> Seq.return "SGVsbG8sIHdvcmxkIQ==" (* "Hello, world!" *)
  | Array typ -> thin depth ([ genall (depth - 1) strhint typ |> Array.of_seq; [||] ] |> List.to_seq)
  | List typ -> thin depth ([ genall (depth - 1) strhint typ |> List.of_seq; [] ] |> List.to_seq)
  | Dict (basic, typ) ->
    let keys = genall (depth - 1) strhint (Basic basic) in
    let vs = genall (depth - 1) strhint typ in
    Seq.product keys vs |> Seq.map (fun x -> [x]) |> thin depth
  | Unit -> Seq.return ()
  | Option t ->
    let vs = genall (depth - 1) strhint t in
    thin depth Seq.(append (map (fun x -> Some x) vs) @@ return None )
  | Tuple (t1, t2) ->
    let v1s = genall (depth - 1) strhint t1 in
    let v2s = genall (depth - 1) strhint t2 in
    Seq.product v1s v2s |> thin depth
  | Tuple3 (t1, t2, t3) ->
    let v1s = genall (depth - 1) strhint t1 in
    let v2s = genall (depth - 1) strhint t2 in
    let v3s = genall (depth - 1) strhint t3 in
    Seq.(product (product v1s v2s) v3s |> map (fun ((x,y),z) -> x,y,z))
  | Tuple4 (t1, t2, t3, t4) ->
    let v1s = genall (depth - 1) strhint t1 in
    let v2s = genall (depth - 1) strhint t2 in
    let v3s = genall (depth - 1) strhint t3 in
    let v4s = genall (depth - 1) strhint t4 in
    Seq.(product (product v1s v2s) (product v3s v4s) |> map (fun ((x,y),(z,t)) -> x,y,z,t))
  | Struct { constructor; fields; _ } ->
    let fields_maxes =
      fields
      |> List.to_seq
      |>
      Seq.map
        (function
          | BoxedField f ->
            let n = Seq.length (genall (depth - 1) strhint f.field) in
            f.fname, n)
    in
    let all_combinations =
      Seq.fold_left
        (fun acc (f, max) ->
          Seq.ints 1 |> Seq.take max |>  Seq.flat_map @@ fun i ->
          Seq.map (fun dict -> (f, i - 1) :: dict) acc
        )
        (Seq.return [] )
        fields_maxes
    in
    Seq.map
      (fun combination ->
        let field_get : type a. string -> a typ -> (a, Rresult.R.msg) Result.t =
         fun fname ty ->
          let n = List.assoc fname combination in
          let vs = genall (depth - 1) fname ty |> Array.of_seq in
          Result.Ok (vs.(n))
        in
        match constructor { field_get } with
        | Result.Ok x -> x
        | Result.Error (`Msg y) -> badstuff y)
      all_combinations
    |> thin depth
  | Variant { variants; _ } ->
    variants
    |> List.to_seq
    |> Seq.flat_map
      (function
        | Rpc.Types.BoxedTag v ->
          let contents = genall (depth - 1) strhint v.tcontents in
          Seq.map (fun content -> v.treview content) contents)
    |> thin depth
  | Abstract { test_data; _ } -> test_data |> List.to_seq


(* don't use this on recursive types! *)

let rec gen_nice : type a. a typ -> string -> a =
 fun ty hint ->
  let narg n = Printf.sprintf "%s_%d" hint n in
  match ty with
  | Basic Int -> 0
  | Basic Int32 -> 0l
  | Basic Int64 -> 0L
  | Basic Bool -> true
  | Basic Float -> 0.0
  | Basic String -> hint
  | Basic Char -> 'a'
  | DateTime -> "19700101T00:00:00Z"
  | Base64 -> "SGVsbG8sIHdvcmxkIQ==" (* "Hello, world!" *)
  | Array typ -> [| gen_nice typ (narg 1); gen_nice typ (narg 2) |]
  | List (Tuple (Basic String, typ)) ->
    [ "field_1", gen_nice typ "value_1"; "field_2", gen_nice typ "value_2" ]
  | List typ -> [ gen_nice typ (narg 1); gen_nice typ (narg 2) ]
  | Dict (String, typ) ->
    [ "field_1", gen_nice typ "value_1"; "field_2", gen_nice typ "value_2" ]
  | Dict (basic, typ) ->
    [ gen_nice (Basic basic) "field_1", gen_nice typ (narg 1)
    ; gen_nice (Basic basic) "field_2", gen_nice typ (narg 2)
    ]
  | Unit -> ()
  | Option ty -> Some (gen_nice ty (Printf.sprintf "optional_%s" hint))
  | Tuple (x, y) -> gen_nice x (narg 1), gen_nice y (narg 2)
  | Tuple3 (x, y, z) -> gen_nice x (narg 1), gen_nice y (narg 2), gen_nice z (narg 3)
  | Tuple4 (x, y, z, a) ->
    gen_nice x (narg 1), gen_nice y (narg 2), gen_nice z (narg 3), gen_nice a (narg 4)
  | Struct { constructor; _ } ->
    let field_get : type a. string -> a typ -> (a, Rresult.R.msg) Result.t =
     fun name ty -> Result.Ok (gen_nice ty name)
    in
    (match constructor { field_get } with
    | Result.Ok x -> x
    | Result.Error (`Msg y) -> badstuff y)
  | Variant { variants; _ } ->
    List.hd variants
    |> (function
    | Rpc.Types.BoxedTag v ->
      let content = gen_nice v.tcontents v.tname in
      v.treview content)
  | Abstract { test_data; _ } -> List.hd test_data

let gentest t = gentest Seen.empty t |> List.of_seq
let genall ?(maxcomb=Sys.max_array_length) depth strhint t = genall ~maxcomb Seen.empty depth strhint t |> List.of_seq
