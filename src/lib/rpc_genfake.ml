(* Generate typed values *)

open Rpc.Types

type err = [`Msg of string]

let badstuff msg =
  failwith (Printf.sprintf "Failed to construct the record: %s" msg)

let rec gentest : type a. a typ -> a list =
 fun t ->
  match t with
  | Basic Int -> [0; 1; max_int; -1; 1000000]
  | Basic Int32 -> [0l; 1l; Int32.max_int; -1l; 999999l]
  | Basic Int64 -> [0L; 1L; Int64.max_int; -1L; 999999999999L]
  | Basic Bool -> [true; false]
  | Basic Float -> [0.0; max_float; min_float; -1.0]
  | Basic String ->
      [ "Test string"
      ; ""
      ; "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ \
         ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ \
         ᚹᛖᛥᚫ"
      ; "\000foo" ]
  | Basic Char -> ['\000'; 'a'; 'z'; '\255']
  | DateTime -> ["19700101T00:00:00Z"]
  | Array typ -> [gentest typ |> Array.of_list; [||]]
  | List typ -> [gentest typ; []]
  | Dict (basic, typ) ->
      let keys = gentest (Basic basic) in
      let vs = gentest typ in
      let x =
        List.fold_left
          (fun (acc, l2) v ->
            match l2 with
            | x :: xs -> ((v, x) :: acc, xs)
            | [] -> ((v, List.hd vs) :: acc, List.tl vs) )
          ([], vs) keys
        |> fst
      in
      [x]
  | Unit -> [()]
  | Option t ->
      let vs = gentest t in
      None :: List.map (fun x -> Some x) vs
  | Tuple (t1, t2) ->
      let v1s = gentest t1 in
      let v2s = gentest t2 in
      List.map (fun v1 -> List.map (fun v2 -> (v1, v2)) v2s) v1s
      |> List.flatten
  | Tuple3 (t1, t2, t3) ->
      let v1s = gentest t1 in
      let v2s = gentest t2 in
      let v3s = gentest t3 in
      List.map
        (fun v1 ->
          List.map (fun v2 -> List.map (fun v3 -> (v1, v2, v3)) v3s) v2s )
        v1s
      |> List.flatten |> List.flatten
  | Tuple4 (t1, t2, t3, t4) ->
      let v1s = gentest t1 in
      let v2s = gentest t2 in
      let v3s = gentest t3 in
      let v4s = gentest t4 in
      List.map
        (fun v1 ->
          List.map
            (fun v2 ->
              List.map
                (fun v3 -> List.map (fun v4 -> (v1, v2, v3, v4)) v4s)
                v3s )
            v2s )
        v1s
      |> List.flatten |> List.flatten |> List.flatten
  | Struct {constructor; _} ->
      let rec gen_n acc n =
        match n with
        | 0 -> acc
        | n ->
            let field_get : type a.
                string -> a typ -> (a, Rresult.R.msg) Result.result =
             fun _ ty ->
              let vs = gentest ty in
              Result.Ok (List.nth vs (Random.int (List.length vs)))
            in
            match constructor {field_get} with
            | Result.Ok x -> gen_n (x :: acc) (n - 1)
            | Result.Error (`Msg y) -> badstuff y
      in
      gen_n [] 10
  | Variant {variants; _} ->
      List.map
        (function
            | Rpc.Types.BoxedTag v ->
                let contents = gentest v.tcontents in
                let content =
                  List.nth contents (Random.int (List.length contents))
                in
                v.treview content)
        variants
  | Abstract {test_data; _} -> test_data
  | Refv (cls,typ) ->
    [NullRef cls; make_ref cls typ "ref"]
  | Refmap typ ->
    let next = gentest typ in
    let numbered = List.fold_left (fun (acc,i) x -> ((string_of_int i,x)::acc, i+1)) ([],0) next in
    let res = List.fold_left (fun acc (ref,x) -> Refmap.add ref x acc) Refmap.empty (fst numbered) in
    [Refmap.empty; res]

let thin d result = if d < 0 then [List.hd result] else result

let rec genall : type a. int -> string -> a typ -> a list =
 fun depth strhint t ->
  match t with
  | Basic Int -> [0]
  | Basic Int32 -> [0l]
  | Basic Int64 -> [0L]
  | Basic Bool -> thin depth [true; false]
  | Basic Float -> [0.0]
  | Basic String -> [strhint]
  | Basic Char -> ['a']
  | DateTime -> ["19700101T00:00:00Z"]
  | Array typ ->
      thin depth [genall (depth - 1) strhint typ |> Array.of_list; [||]]
  | List typ -> thin depth [genall (depth - 1) strhint typ; []]
  | Dict (basic, typ) ->
      let keys = genall (depth - 1) strhint (Basic basic) in
      let vs = genall (depth - 1) strhint typ in
      let x = List.map (fun k -> List.map (fun v -> [(k, v)]) vs) keys in
      List.flatten x |> thin depth
  | Unit -> [()]
  | Option t ->
      let vs = genall (depth - 1) strhint t in
      thin depth (List.map (fun x -> Some x) vs @ [None])
  | Tuple (t1, t2) ->
      let v1s = genall (depth - 1) strhint t1 in
      let v2s = genall (depth - 1) strhint t2 in
      List.map (fun v1 -> List.map (fun v2 -> (v1, v2)) v2s) v1s
      |> List.flatten |> thin depth
  | Tuple3 (t1, t2, t3) ->
      let v1s = genall (depth - 1) strhint t1 in
      let v2s = genall (depth - 1) strhint t2 in
      let v3s = genall (depth - 1) strhint t3 in
      let l =
        List.map
          (fun v1 ->
            List.map (fun v2 -> List.map (fun v3 -> (v1, v2, v3)) v3s) v2s )
          v1s
      in
      l |> List.flatten |> List.flatten |> thin depth
  | Tuple4 (t1, t2, t3, t4) ->
      let v1s = genall (depth - 1) strhint t1 in
      let v2s = genall (depth - 1) strhint t2 in
      let v3s = genall (depth - 1) strhint t3 in
      let v4s = genall (depth - 1) strhint t4 in
      let l =
        List.map
          (fun v1 ->
            List.map
              (fun v2 ->
                List.map
                  (fun v3 -> List.map (fun v4 -> (v1, v2, v3, v4)) v4s)
                  v3s )
              v2s )
          v1s
      in
      l |> List.flatten |> List.flatten |> List.flatten |> thin depth
  | Struct {constructor; fields; _} ->
      let fields_maxes =
        List.map
          (function
              | BoxedField f ->
                  let n = List.length (genall (depth - 1) strhint f.field) in
                  (String.concat "." f.fname, n))
          fields
      in
      let all_combinations =
        List.fold_left
          (fun acc (f, max) ->
            let rec inner n = if n = 0 then [] else (f, n) :: inner (n - 1) in
            let ns = inner max in
            List.map
              (fun (f, n) -> List.map (fun dict -> (f, n - 1) :: dict) acc)
              ns
            |> List.flatten )
          [[]] fields_maxes
      in
      List.map
        (fun combination ->
          let field_get : type a.
              string -> a typ -> (a, Rresult.R.msg) Result.result =
           fun fname ty ->
            let n = List.assoc fname combination in
            let vs = genall (depth - 1) fname ty in
            Result.Ok (List.nth vs n)
          in
          match constructor {field_get} with
          | Result.Ok x -> x
          | Result.Error (`Msg y) -> badstuff y )
        all_combinations
      |> thin depth
  | Variant {variants; _} ->
      List.map
        (function
            | Rpc.Types.BoxedTag v ->
                let contents = genall (depth - 1) strhint v.tcontents in
                List.map (fun content -> v.treview content) contents)
        variants
      |> List.flatten |> thin depth
  | Abstract {test_data; _} -> test_data
  | Refv (cls,typ) ->
    List.init depth (fun i -> make_ref cls typ (Printf.sprintf "%d" i))
  | Refmap typ ->
    let next = genall (depth - 1) strhint typ in
    let numbered = List.fold_left (fun (acc,i) x -> ((string_of_int i,x)::acc, i+1)) ([],0) next in
    let res = List.fold_left (fun acc (ref,x) -> Refmap.add ref x acc) Refmap.empty (fst numbered) in
    [res]

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
  | Array typ -> [|gen_nice typ (narg 1); gen_nice typ (narg 2)|]
  | List (Tuple (Basic String, typ)) ->
      [("field_1", gen_nice typ "value_1"); ("field_2", gen_nice typ "value_2")]
  | List typ -> [gen_nice typ (narg 1); gen_nice typ (narg 2)]
  | Dict (String, typ) ->
      [("field_1", gen_nice typ "value_1"); ("field_2", gen_nice typ "value_2")]
  | Dict (basic, typ) ->
      [ (gen_nice (Basic basic) "field_1", gen_nice typ (narg 1))
      ; (gen_nice (Basic basic) "field_2", gen_nice typ (narg 2)) ]
  | Unit -> ()
  | Option ty -> Some (gen_nice ty (Printf.sprintf "optional_%s" hint))
  | Tuple (x, y) -> (gen_nice x (narg 1), gen_nice y (narg 2))
  | Tuple3 (x, y, z) ->
      (gen_nice x (narg 1), gen_nice y (narg 2), gen_nice z (narg 3))
  | Tuple4 (x, y, z, a) ->
      ( gen_nice x (narg 1)
      , gen_nice y (narg 2)
      , gen_nice z (narg 3)
      , gen_nice a (narg 4) )
  | Struct {constructor; _} -> (
      let field_get : type a.
          string -> a typ -> (a, Rresult.R.msg) Result.result =
       fun name ty -> Result.Ok (gen_nice ty name)
      in
      match constructor {field_get} with
      | Result.Ok x -> x
      | Result.Error (`Msg y) -> badstuff y )
  | Variant {variants; _} -> (
      List.hd variants
      |> function
        | Rpc.Types.BoxedTag v ->
            let content = gen_nice v.tcontents v.tname in
            v.treview content )
  | Abstract {test_data; _} -> List.hd test_data
  | Refv (cls,typ) ->
    make_ref cls typ "ref"
  | Refmap typ ->
    let next = gen_nice typ hint in
    Refmap.add "ref0" next Refmap.empty
