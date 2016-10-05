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

(** {2 Value} *)

type t =
    Int of int64
  | Int32 of int32
  | Bool of bool
  | Float of float
  | String of string
  | DateTime of string
  | Enum of t list
  | Dict of (string * t) list
  | Null

val to_string : t -> string

module Version : sig
  type t = int * int * int

  val compare : t -> t -> int
end

(** {2 Type declarations} *)
module Types : sig
  type _ basic =
      Int : int basic
    | Int32 : int32 basic
    | Int64 : int64 basic
    | Bool : bool basic
    | Float : float basic
    | String : string basic
    | Char : char basic
  type _ typ =
      Basic : 'a basic -> 'a typ
    | DateTime : string typ
    | Array : 'a typ -> 'a array typ
    | List : 'a typ -> 'a list typ
    | Dict : 'a basic * 'b typ -> ('a * 'b) list typ
    | Unit : unit typ
    | Option : 'a typ -> 'a option typ
    | Tuple : 'a typ * 'b typ -> ('a * 'b) typ
    | Struct : 'a structure -> 'a typ
    | Variant : 'a variant -> 'a typ
  and 'a def = { name : string; description : string; ty : 'a typ; }
  and boxed_def = BoxedDef : 'a def -> boxed_def
  and ('a, 's) field = {
    fname : string;
    fdescription : string;
    fversion : Version.t option;
    field : 'a typ;
    fget : 's -> 'a;
    fset : 'a -> 's -> 's;
  }
  and 'a boxed_field = BoxedField : ('a, 's) field -> 's boxed_field
  and field_getter = {
    g : 'a. string -> 'a typ -> ('a, Rresult.R.msg) Result.result;
  }
  and 'a structure = {
    sname : string;
    mutable fields : 'a boxed_field list;
    constructor : field_getter -> ('a, Rresult.R.msg) Result.result;
  }
  and ('a, 's) tag = {
    vname : string;
    vdescription : string;
    vversion : Version.t option;
    vcontents : 'a typ;
    vpreview : 's -> 'a option; (* Prism *)
    vreview : 'a -> 's;
  }
  and 'a boxed_tag = BoxedTag : ('a, 's) tag -> 's boxed_tag
  and tag_getter = {
    t : 'a. 'a typ -> ('a, Rresult.R.msg) Result.result;
  }
  and 'a variant = {
    mutable variants : 'a boxed_tag list;
    vconstructor : string -> tag_getter -> ('a, Rresult.R.msg) Result.result;
  }
  val int : int def
  val int32 : int32 def
  val int64 : int64 def
  val bool : bool def
  val float : float def
  val string : string def
  val char : char def
  val unit : unit def
end

(** {2 Basic constructors} *)

val int64_of_rpc : t -> (int64, Rresult.R.msg) Result.result
val rpc_of_int64 : int64 -> t

val int32_of_rpc : t -> (int32, Rresult.R.msg) Result.result
val rpc_of_int32 : int32 -> t

val int_of_rpc : t -> (int, Rresult.R.msg) Result.result
val rpc_of_int : int -> t

val bool_of_rpc : t -> (bool, Rresult.R.msg) Result.result
val rpc_of_bool : bool -> t

val float_of_rpc : t -> (float, Rresult.R.msg) Result.result
val rpc_of_float : float -> t

val string_of_rpc : t -> (string, Rresult.R.msg) Result.result
val rpc_of_string : string -> t

val dateTime_of_rpc : t -> (string, Rresult.R.msg) Result.result
val rpc_of_dateTime : string -> t

val t_of_rpc : t -> (t, Rresult.R.msg) Result.result (* For consistency!? *)
val rpc_of_t : t -> t

val unit_of_rpc : t -> (unit, Rresult.R.msg) Result.result
val rpc_of_unit : unit -> t

val char_of_rpc : t -> (char, Rresult.R.msg) Result.result
val rpc_of_char : char -> t

(** {2 Backwards compatibility module} *)
module ExnProducing : sig
type rpc = t
type t = rpc
val int64_of_rpc : t -> int64
val int32_of_rpc : t -> int32
val int_of_rpc : t -> int
val bool_of_rpc : t -> bool
val float_of_rpc : t -> float
val string_of_rpc : t -> string
val dateTime_of_rpc : t -> string
val t_of_rpc : t -> t
val rpc_of_t : t -> t
val unit_of_rpc : t -> unit
end

(** {2 Calls} *)

type callback = string list -> t -> unit

type call = { name : string; params : t list }

val call : string -> t list -> call

val string_of_call : call -> string

(** {2 Responses} *)

type response = { success : bool; contents : t }

val string_of_response : response -> string

val success : t -> response
val failure : t -> response

(** {2 Run-time errors} *)

exception Runtime_error of string * t
exception Runtime_exception of string * string

(** {2 Debug options} *)
val set_debug : bool -> unit
val get_debug : unit -> bool

(** Helper *)
val lowerfn : t -> t
val map_bind : ('a -> ('b, 'c) Result.result) -> 'b list -> 'a list -> ('b list, 'c) Result.result
(** [struct_extend rpc1 rpc2] first checks that [rpc1] and [rpc2] are both
 *  dictionaries. If this is the case then [struct_extend] will create a new
 *  [Rpc.t] which contains all key-value pairs from [rpc1], as well as all
 *  key-value pairs from [rpc2] for which the key does not exist in [rpc1]. *)
val struct_extend : t -> t -> t
