(* Generate typed values *)

open Rpc.Types

type err = [ `Msg of string ]

let rec gentest : type a. a typ -> a list  = fun t ->
  let open Rpc in
  match t with
  | Basic Int -> [0; 1; max_int; -1; 1000000;]
  | Basic Int32 -> [0l; 1l; Int32.max_int; -1l; 999999l]
  | Basic Int64 -> [0L; 1L; Int64.max_int; -1L; 999999999999L]
  | Basic Bool -> [true; false]
  | Basic Float -> [0.0; max_float; min_float; -1.0]
  | Basic String -> ["Test string"; ""; "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"; "\000foo"]
  | Basic Char -> ['\000'; 'a'; 'z'; '\255']
  | DateTime -> ["19700101T00:00:00Z"]
  | Array typ -> [gentest typ |> Array.of_list; [||]]
  | List typ -> [gentest typ; []]
  | Dict (basic, typ) ->
    let keys = gentest (Basic basic) in
    let vs = gentest typ in
    let x = List.fold_left (fun (acc,l2) v -> match l2 with x::xs -> (v,x)::acc, xs | [] -> (v, List.hd vs)::acc, List.tl vs) ([],vs) keys |> fst in
    [x]
  | Unit -> [()]
  | Option t ->
    let vs = gentest t in
    None :: List.map (fun x -> Some x) vs
  | Tuple (t1, t2) ->
    let v1s = gentest t1 in
    let v2s = gentest t2 in
    List.map (fun v1 -> List.map (fun v2 -> (v1,v2)) v2s) v1s |> List.flatten
  | Struct { constructor; sname } ->
    let rec gen_n acc n =
      match n with
      | 0 -> acc
      | n ->
        let fget : type a. string -> a typ -> (a, Rresult.R.msg) Result.result  = fun _ ty ->
          let vs = gentest ty in
          Result.Ok (List.nth vs (Random.int (List.length vs)))
        in
        match constructor { fget } with Result.Ok x -> gen_n (x::acc) (n-1) | Result.Error y -> failwith "Bad stuff"
    in gen_n [] 10
  | Variant { vconstructor; variants } ->
    List.map (function Rpc.Types.BoxedTag v ->
        let contents = gentest v.tcontents in
        let content = List.nth contents (Random.int (List.length contents)) in
        v.treview content) variants
  | Abstract _ -> failwith "Abstract types not supported by rpc_genfake"


let thin d result =
  if d < 0
  then [List.hd result]
  else result

let rec genall : type a. int -> string -> a typ -> a list  = fun depth strhint t ->
  let open Rpc in
  match t with
  | Basic Int -> [0]
  | Basic Int32 -> [0l]
  | Basic Int64 -> [0L]
  | Basic Bool -> thin depth [true; false]
  | Basic Float -> [0.0]
  | Basic String -> [strhint]
  | Basic Char -> ['a']
  | DateTime -> ["19700101T00:00:00Z"]
  | Array typ -> thin depth [genall (depth - 1) strhint typ |> Array.of_list; [||]]
  | List typ -> thin depth [genall (depth - 1) strhint typ; []]
  | Dict (basic, typ) ->
    let keys = genall (depth - 1) strhint (Basic basic) in
    let vs = genall (depth - 1) strhint typ in
    let x = List.map (fun k ->
        List.map (fun v -> [(k,v)]) vs
      ) keys in
    List.flatten x |> thin depth
  | Unit -> [()]
  | Option t ->
    let vs = genall (depth - 1) strhint t in
    thin depth ((List.map (fun x -> Some x) vs) @ [None])
  | Tuple (t1, t2) ->
    let v1s = genall (depth - 1) strhint t1 in
    let v2s = genall (depth - 1) strhint t2 in
    List.map (fun v1 -> List.map (fun v2 -> (v1,v2)) v2s) v1s |> List.flatten |> thin depth
  | Struct { constructor; sname; fields} ->
    let fields_maxes = List.map (function BoxedField f -> let n = List.length (genall (depth - 1) strhint f.field) in (f.fname,n)) fields in
    let all_combinations = List.fold_left (fun acc (f,max) ->
        let rec inner n = if n=0 then [] else (f,n)::inner (n-1) in
        let ns = inner max in
        List.map (fun (f,n) -> List.map (fun dict -> (f,(n-1))::dict) acc) ns |> List.flatten
      ) [[]] fields_maxes in
    List.map (fun combination ->
      let fget : type a. string -> a typ -> (a, Rresult.R.msg) Result.result  = fun fname ty ->
        let n = List.assoc fname combination in
        let vs = genall (depth - 1) fname ty in
        Result.Ok (List.nth vs n)
      in
      match constructor { fget } with Result.Ok x -> x | Result.Error y -> failwith "Bad stuff") all_combinations |> thin depth
  | Variant { vconstructor; variants } ->
    List.map (function Rpc.Types.BoxedTag v ->
        let contents = genall (depth - 1) strhint v.tcontents in
        List.map (fun content -> v.treview content) contents) variants |> List.flatten |> thin depth
  | Abstract _ -> failwith "Abstract types not supported by rpc_genfake"
