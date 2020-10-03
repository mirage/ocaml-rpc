(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2006-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Rpc

module Yojson_private = struct
  include Yojson.Safe

  let from_string ?(strict = true) ?buf ?fname ?lnum s =
    let open Yojson in
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      if strict then from_lexbuf v lexbuf else from_lexbuf v ~stream:true lexbuf
    with
    | End_of_input -> json_error "Blank input data"
end

module Y = Yojson_private
module U = Yojson.Basic.Util

type version =
  | V1
  | V2

let rec rpc_to_json t =
  match t with
  | Int i -> `Intlit (Int64.to_string i)
  | Int32 i -> `Int (Int32.to_int i)
  | Bool b -> `Bool b
  | Float r -> `Float r
  | String s -> `String s
  | DateTime d -> `String d
  | Base64 b -> `String b
  | Null -> `Null
  | Enum a -> `List (Rpcmarshal.tailrec_map rpc_to_json a)
  | Dict a -> `Assoc (Rpcmarshal.tailrec_map (fun (k, v) -> k, rpc_to_json v) a)


exception JsonToRpcError of Y.t

let rec json_to_rpc t =
  match t with
  | `Intlit i -> Int (Int64.of_string i)
  | `Int i -> Int (Int64.of_int i)
  | `Bool b -> Bool b
  | `Float r -> Float r
  | `String s -> (* TODO: check if it is a DateTime *) String s
  (* | DateTime d -> `String d *)
  (* | Base64 b -> `String b *)
  | `Null -> Null
  | `List a -> Enum (Rpcmarshal.tailrec_map json_to_rpc a)
  | `Assoc a -> Dict (Rpcmarshal.tailrec_map (fun (k, v) -> k, json_to_rpc v) a)
  | unsupported -> raise (JsonToRpcError unsupported)


let to_fct t f = rpc_to_json t |> Y.to_string |> f
let to_buffer t buf = to_fct t (fun s -> Buffer.add_string buf s)
let to_string t = rpc_to_json t |> Y.to_string

let to_a ~empty ~append t =
  let buf = empty () in
  to_fct t (fun s -> append buf s);
  buf


let new_id =
  let count = ref 0L in
  fun () ->
    count := Int64.add 1L !count;
    !count


let string_of_call ?(version = V1) call =
  let json =
    match version with
    | V1 -> [ "method", String call.name; "params", Enum call.params ]
    | V2 ->
      let params =
        match call.params with
        | [ Dict x ] -> Dict x
        | _ -> Enum call.params
      in
      [ "jsonrpc", String "2.0"; "method", String call.name; "params", params ]
  in
  let json =
    if not call.is_notification then json @ [ "id", Int (new_id ()) ] else json
  in
  to_string (Dict json)


let json_of_response ?(id = Int 0L) version response =
  if response.Rpc.success
  then (
    match version with
    | V1 -> Dict [ "result", response.Rpc.contents; "error", Null; "id", id ]
    | V2 -> Dict [ "jsonrpc", String "2.0"; "result", response.Rpc.contents; "id", id ])
  else (
    match version with
    | V1 -> Dict [ "result", Null; "error", response.Rpc.contents; "id", id ]
    | V2 -> Dict [ "jsonrpc", String "2.0"; "error", response.Rpc.contents; "id", id ])


let json_of_error_object ?(data = None) code message =
  let data_json =
    match data with
    | Some d -> [ "data", d ]
    | None -> []
  in
  Dict ([ "code", Int code; "message", String message ] @ data_json)


let string_of_response ?(id = Int 0L) ?(version = V1) response =
  let json = json_of_response ~id version response in
  to_string json


let a_of_response ?(id = Int 0L) ?(version = V1) ~empty ~append response =
  let json = json_of_response ~id version response in
  to_a ~empty ~append json


let of_string ?(strict = true) s = s |> Y.from_string ~strict |> json_to_rpc

let of_a ~next_char b =
  let buf = Buffer.create 2048 in
  let rec acc () =
    match next_char b with
    | Some c ->
      Buffer.add_char buf c;
      acc ()
    | None -> ()
  in
  acc ();
  Buffer.contents buf |> of_string


let get' name dict =
  try Some (List.assoc name dict) with
  | Not_found -> None


exception Malformed_method_request of string
exception Malformed_method_response of string
exception Missing_field of string

let get name dict =
  match get' name dict with
  | None ->
    if Rpc.get_debug () then Printf.eprintf "%s was not found in the dictionary\n" name;
    raise (Missing_field name)
  | Some v -> v


let version_id_and_call_of_string_option str =
  try
    match of_string str with
    | Dict d ->
      let name =
        match get "method" d with
        | String s -> s
        | _ -> raise (Malformed_method_request "Invalid field 'method' in request body")
      in
      let version =
        match get' "jsonrpc" d with
        | None -> V1
        | Some (String "2.0") -> V2
        | _ -> raise (Malformed_method_request "Invalid field 'jsonrpc' in request body")
      in
      let params =
        match version with
        | V1 ->
          (match get "params" d with
          | Enum l -> l
          | _ -> raise (Malformed_method_request "Invalid field 'params' in request body"))
        | V2 ->
          (match get' "params" d with
          | None | Some Null -> []
          | Some (Enum l) -> l
          | Some (Dict l) -> [ Dict l ]
          | _ -> raise (Malformed_method_request "Invalid field 'params' in request body"))
      in
      let id =
        match get' "id" d with
        | None | Some Null -> None (* is a notification *)
        | Some (Int a) -> Some (Int a)
        | Some (String a) -> Some (String a)
        | Some _ -> raise (Malformed_method_request "Invalid field 'id' in request body")
      in
      let c = call name params in
      version, id, { c with is_notification = id == None }
    | _ -> raise (Malformed_method_request "Invalid request body")
  with
  | Missing_field field ->
    raise (Malformed_method_request (Printf.sprintf "Required field %s is missing" field))
  | JsonToRpcError json ->
    raise
      (Malformed_method_request (Printf.sprintf "Unable to parse %s" (Y.to_string json)))


let version_id_and_call_of_string s =
  let version, id_, call = version_id_and_call_of_string_option s in
  match id_ with
  | Some id -> version, id, call
  | None -> raise (Malformed_method_request "Invalid field 'id' in request body")


let call_of_string str =
  let _, _, call = version_id_and_call_of_string str in
  call


(* This functions parses the json and tries to extract a valid jsonrpc response
 * (See http://www.jsonrpc.org/ for the exact specs). *)
let get_response extractor str =
  try
    match extractor str with
    | Dict d ->
      let _ =
        match get "id" d with
        | Int _ as x -> x
        | String _ as y -> y
        | _ -> raise (Malformed_method_response "id")
      in
      (match get' "jsonrpc" d with
      | None ->
        let result = get "result" d in
        let error = get "error" d in
        (match result, error with
        | v, Null -> success v
        | Null, v -> failure v
        | x, y ->
          raise
            (Malformed_method_response
               (Printf.sprintf
                  "<result=%s><error=%s>"
                  (Rpc.to_string x)
                  (Rpc.to_string y))))
      | Some (String "2.0") ->
        let result = get' "result" d in
        let error = get' "error" d in
        (match result, error with
        | Some v, None -> success v
        | None, Some v ->
          (match v with
          | Dict err ->
            let (_ : int64) =
              match get "code" err with
              | Int i -> i
              | _ -> raise (Malformed_method_response "Error code")
            in
            let _ =
              match get "message" err with
              | String s -> s
              | _ -> raise (Malformed_method_response "Error message")
            in
            failure v
          | _ -> raise (Malformed_method_response "Error object"))
        | Some x, Some y ->
          raise
            (Malformed_method_response
               (Printf.sprintf
                  "<result=%s><error=%s>"
                  (Rpc.to_string x)
                  (Rpc.to_string y)))
        | None, None ->
          raise
            (Malformed_method_response
               (Printf.sprintf "neither <result> nor <error> was found")))
      | _ -> raise (Malformed_method_response "jsonrpc"))
    | rpc ->
      raise
        (Malformed_method_response
           (Printf.sprintf "<response_of_stream(%s)>" (to_string rpc)))
  with
  | Missing_field field ->
    raise (Malformed_method_response (Printf.sprintf "<%s was not found>" field))
  | JsonToRpcError json ->
    raise
      (Malformed_method_response
         (Printf.sprintf "<unable to parse %s>" (Y.to_string json)))


let response_of_string ?(strict = true) str = get_response (of_string ~strict) str

let response_of_in_channel channel =
  let of_channel s = s |> Y.from_channel |> json_to_rpc in
  get_response of_channel channel
