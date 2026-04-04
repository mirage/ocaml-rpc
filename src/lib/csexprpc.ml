(*
 * Copyright (c) Cloud Software Group, Inc.
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

open Sexplib0.Sexp_conv

(** {1 Auto-generated conversions between {!module:Rpc} types and S-expressions} *)

(** {2 {!type:Rpc.t} conversion} *)

type t = Rpc.t =
  | Int of int64
  | Int32 of int32
  | Bool of bool
  | Float of float
  | String of string
  | DateTime of string
  | Enum of t list
  | Dict of (string * t) list
  | Base64 of string
  | Null
(* code below updated by [dune build @lint --auto-promote] *)
[@@deriving_inline sexp]

let _ = fun (_ : t) -> ()

let rec t_of_sexp =
  (let error_source__003_ = "src/lib/csexprpc.ml.t" in
   function
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("int" | "Int") as _tag__006_) :: sexp_args__007_) as
     _sexp__005_ ->
     (match sexp_args__007_ with
      | arg0__008_ :: [] ->
        let res0__009_ = int64_of_sexp arg0__008_ in
        Int res0__009_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__006_
          _sexp__005_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("int32" | "Int32") as _tag__011_) :: sexp_args__012_) as
     _sexp__010_ ->
     (match sexp_args__012_ with
      | arg0__013_ :: [] ->
        let res0__014_ = int32_of_sexp arg0__013_ in
        Int32 res0__014_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__011_
          _sexp__010_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("bool" | "Bool") as _tag__016_) :: sexp_args__017_) as
     _sexp__015_ ->
     (match sexp_args__017_ with
      | arg0__018_ :: [] ->
        let res0__019_ = bool_of_sexp arg0__018_ in
        Bool res0__019_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__016_
          _sexp__015_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("float" | "Float") as _tag__021_) :: sexp_args__022_) as
     _sexp__020_ ->
     (match sexp_args__022_ with
      | arg0__023_ :: [] ->
        let res0__024_ = float_of_sexp arg0__023_ in
        Float res0__024_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__021_
          _sexp__020_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("string" | "String") as _tag__026_) :: sexp_args__027_) as
     _sexp__025_ ->
     (match sexp_args__027_ with
      | arg0__028_ :: [] ->
        let res0__029_ = string_of_sexp arg0__028_ in
        String res0__029_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__026_
          _sexp__025_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("dateTime" | "DateTime") as _tag__031_) :: sexp_args__032_)
     as _sexp__030_ ->
     (match sexp_args__032_ with
      | arg0__033_ :: [] ->
        let res0__034_ = string_of_sexp arg0__033_ in
        DateTime res0__034_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__031_
          _sexp__030_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("enum" | "Enum") as _tag__036_) :: sexp_args__037_) as
     _sexp__035_ ->
     (match sexp_args__037_ with
      | arg0__038_ :: [] ->
        let res0__039_ = list_of_sexp t_of_sexp arg0__038_ in
        Enum res0__039_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__036_
          _sexp__035_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("dict" | "Dict") as _tag__041_) :: sexp_args__042_) as
     _sexp__040_ ->
     (match sexp_args__042_ with
      | arg0__048_ :: [] ->
        let res0__049_ =
          list_of_sexp
            (function
              | Sexplib0.Sexp.List [ arg0__043_; arg1__044_ ] ->
                let res0__045_ = string_of_sexp arg0__043_
                and res1__046_ = t_of_sexp arg1__044_ in
                res0__045_, res1__046_
              | sexp__047_ ->
                Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                  error_source__003_
                  2
                  sexp__047_)
            arg0__048_
        in
        Dict res0__049_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__041_
          _sexp__040_)
   | Sexplib0.Sexp.List
       (Sexplib0.Sexp.Atom (("base64" | "Base64") as _tag__051_) :: sexp_args__052_) as
     _sexp__050_ ->
     (match sexp_args__052_ with
      | arg0__053_ :: [] ->
        let res0__054_ = string_of_sexp arg0__053_ in
        Base64 res0__054_
      | _ ->
        Sexplib0.Sexp_conv_error.stag_incorrect_n_args
          error_source__003_
          _tag__051_
          _sexp__050_)
   | Sexplib0.Sexp.Atom ("null" | "Null") -> Null
   | Sexplib0.Sexp.Atom ("int" | "Int") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("int32" | "Int32") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("bool" | "Bool") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("float" | "Float") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("string" | "String") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("dateTime" | "DateTime") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("enum" | "Enum") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("dict" | "Dict") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.Atom ("base64" | "Base64") as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("null" | "Null") :: _) as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__002_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__003_ sexp__002_
   | Sexplib0.Sexp.List [] as sexp__002_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__003_ sexp__002_
   | sexp__002_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__003_ sexp__002_
   : Sexplib0.Sexp.t -> t)


let _ = t_of_sexp

let rec sexp_of_t =
  (function
   | Int arg0__055_ ->
     let res0__056_ = sexp_of_int64 arg0__055_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Int"; res0__056_ ]
   | Int32 arg0__057_ ->
     let res0__058_ = sexp_of_int32 arg0__057_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Int32"; res0__058_ ]
   | Bool arg0__059_ ->
     let res0__060_ = sexp_of_bool arg0__059_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Bool"; res0__060_ ]
   | Float arg0__061_ ->
     let res0__062_ = sexp_of_float arg0__061_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Float"; res0__062_ ]
   | String arg0__063_ ->
     let res0__064_ = sexp_of_string arg0__063_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "String"; res0__064_ ]
   | DateTime arg0__065_ ->
     let res0__066_ = sexp_of_string arg0__065_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "DateTime"; res0__066_ ]
   | Enum arg0__067_ ->
     let res0__068_ = sexp_of_list sexp_of_t arg0__067_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Enum"; res0__068_ ]
   | Dict arg0__073_ ->
     let res0__074_ =
       sexp_of_list
         (fun (arg0__069_, arg1__070_) ->
            let res0__071_ = sexp_of_string arg0__069_
            and res1__072_ = sexp_of_t arg1__070_ in
            Sexplib0.Sexp.List [ res0__071_; res1__072_ ])
         arg0__073_
     in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Dict"; res0__074_ ]
   | Base64 arg0__075_ ->
     let res0__076_ = sexp_of_string arg0__075_ in
     Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Base64"; res0__076_ ]
   | Null -> Sexplib0.Sexp.Atom "Null"
   : t -> Sexplib0.Sexp.t)


let _ = sexp_of_t

[@@@deriving.end]

(** {2 {!type:Rpc.call} conversion} *)
type call = Rpc.call =
  { name : string
  ; params : t list
  ; is_notification : bool
  }
(* code below updated by [dune build @lint --auto-promote] *)
[@@deriving_inline sexp]

let _ = fun (_ : call) -> ()

let call_of_sexp =
  (let error_source__078_ = "src/lib/csexprpc.ml.call" in
   fun x__079_ ->
     Sexplib0.Sexp_conv_record.record_of_sexp
       ~caller:error_source__078_
       ~fields:
         (Field
            { name = "name"
            ; kind = Required
            ; conv = string_of_sexp
            ; rest =
                Field
                  { name = "params"
                  ; kind = Required
                  ; conv = list_of_sexp t_of_sexp
                  ; rest =
                      Field
                        { name = "is_notification"
                        ; kind = Required
                        ; conv = bool_of_sexp
                        ; rest = Empty
                        }
                  }
            })
       ~index_of_field:(function
         | "name" -> 0
         | "params" -> 1
         | "is_notification" -> 2
         | _ -> -1)
       ~allow_extra_fields:false
       ~create:(fun (name, (params, (is_notification, ()))) ->
         ({ name; params; is_notification } : call))
       x__079_
   : Sexplib0.Sexp.t -> call)


let _ = call_of_sexp

let sexp_of_call =
  (fun { name = name__081_
       ; params = params__083_
       ; is_notification = is_notification__085_
       } ->
     let bnds__080_ = ([] : _ Stdlib.List.t) in
     let bnds__080_ =
       let arg__086_ = sexp_of_bool is_notification__085_ in
       (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "is_notification"; arg__086_ ]
        :: bnds__080_
        : _ Stdlib.List.t)
     in
     let bnds__080_ =
       let arg__084_ = sexp_of_list sexp_of_t params__083_ in
       (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "params"; arg__084_ ] :: bnds__080_
        : _ Stdlib.List.t)
     in
     let bnds__080_ =
       let arg__082_ = sexp_of_string name__081_ in
       (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "name"; arg__082_ ] :: bnds__080_
        : _ Stdlib.List.t)
     in
     Sexplib0.Sexp.List bnds__080_
   : call -> Sexplib0.Sexp.t)


let _ = sexp_of_call

[@@@deriving.end]

(** {2 {!type:Rpc.response} conversion} *)
type response = Rpc.response =
  { success : bool
  ; contents : t
  ; is_notification : bool
  }
[@@deriving_inline sexp]

let _ = fun (_ : response) -> ()

let response_of_sexp =
  (let error_source__088_ = "src/lib/csexprpc.ml.response" in
   fun x__089_ ->
     Sexplib0.Sexp_conv_record.record_of_sexp
       ~caller:error_source__088_
       ~fields:
         (Field
            { name = "success"
            ; kind = Required
            ; conv = bool_of_sexp
            ; rest =
                Field
                  { name = "contents"
                  ; kind = Required
                  ; conv = t_of_sexp
                  ; rest =
                      Field
                        { name = "is_notification"
                        ; kind = Required
                        ; conv = bool_of_sexp
                        ; rest = Empty
                        }
                  }
            })
       ~index_of_field:(function
         | "success" -> 0
         | "contents" -> 1
         | "is_notification" -> 2
         | _ -> -1)
       ~allow_extra_fields:false
       ~create:(fun (success, (contents, (is_notification, ()))) ->
         ({ success; contents; is_notification } : response))
       x__089_
   : Sexplib0.Sexp.t -> response)


let _ = response_of_sexp

let sexp_of_response =
  (fun { success = success__091_
       ; contents = contents__093_
       ; is_notification = is_notification__095_
       } ->
     let bnds__090_ = ([] : _ Stdlib.List.t) in
     let bnds__090_ =
       let arg__096_ = sexp_of_bool is_notification__095_ in
       (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "is_notification"; arg__096_ ]
        :: bnds__090_
        : _ Stdlib.List.t)
     in
     let bnds__090_ =
       let arg__094_ = sexp_of_t contents__093_ in
       (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "contents"; arg__094_ ] :: bnds__090_
        : _ Stdlib.List.t)
     in
     let bnds__090_ =
       let arg__092_ = sexp_of_bool success__091_ in
       (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "success"; arg__092_ ] :: bnds__090_
        : _ Stdlib.List.t)
     in
     Sexplib0.Sexp.List bnds__090_
   : response -> Sexplib0.Sexp.t)


let _ = sexp_of_response

[@@@deriving.end]

(** {2 exception converter} *)

exception
  Parse_error of
    { offset : int
    ; message : string
    ; input : string
    }
[@@deriving_inline sexp_of]

let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Parse_error] (function
    | Parse_error { offset = offset__098_; message = message__100_; input = input__102_ }
      ->
      let bnds__097_ = ([] : _ Stdlib.List.t) in
      let bnds__097_ =
        let arg__103_ = sexp_of_string input__102_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "input"; arg__103_ ] :: bnds__097_
         : _ Stdlib.List.t)
      in
      let bnds__097_ =
        let arg__101_ = sexp_of_string message__100_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "message"; arg__101_ ] :: bnds__097_
         : _ Stdlib.List.t)
      in
      let bnds__097_ =
        let arg__099_ = sexp_of_int offset__098_ in
        (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "offset"; arg__099_ ] :: bnds__097_
         : _ Stdlib.List.t)
      in
      Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom "src/lib/csexprpc.ml.Parse_error" :: bnds__097_)
    | _ -> assert false)


[@@@deriving.end]

(** {1 Convert S-expressions to and from strings using {!module:Csexp}, an efficient, yet still human-readable encoding.} *)

module Wire = Csexp.Make (Sexplib0.Sexp)

let wire_of_sexp = Wire.to_string

let sexp_of_wire input =
  match Wire.parse_string input with
  | Ok r -> r
  | Error (offset, message) -> raise (Parse_error { offset; message; input })


(** {1 Convert {!module:Rpc} types to/from string} *)

let to_string t = t |> sexp_of_t |> wire_of_sexp
let of_string s = s |> sexp_of_wire |> t_of_sexp
let string_of_call call = call |> sexp_of_call |> wire_of_sexp
let call_of_string s = s |> sexp_of_wire |> call_of_sexp
let string_of_response response = response |> sexp_of_response |> wire_of_sexp
let response_of_string s = s |> sexp_of_wire |> response_of_sexp
