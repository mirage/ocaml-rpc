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

type 'a error_or = ('a, string) Result.result

val bind : 'a error_or -> ('a -> 'b error_or) -> 'b error_or
val return : 'a -> 'a error_or
val (>>=) : 'a error_or -> ('a -> 'b error_or) -> 'b error_or
val map_bind : ('a -> 'b error_or) -> 'b list -> 'a list -> ('b list) error_or

val to_string : t -> string

(** {2 Basic constructors} *)

val int64_of_rpc : t -> int64 error_or
val rpc_of_int64 : int64 -> t

val int32_of_rpc : t -> int32 error_or
val rpc_of_int32 : int32 -> t

val int_of_rpc : t -> int error_or
val rpc_of_int : int -> t

val bool_of_rpc : t -> bool error_or
val rpc_of_bool : bool -> t

val float_of_rpc : t -> float error_or
val rpc_of_float : float -> t

val string_of_rpc : t -> string error_or
val rpc_of_string : string -> t

val dateTime_of_rpc : t -> string error_or
val rpc_of_dateTime : string -> t

val t_of_rpc : t -> t error_or (* For consistency!? *)
val rpc_of_t : t -> t

val unit_of_rpc : t -> unit error_or
val rpc_of_unit : unit -> t

module ExnProducing : sig
val int64_of_rpc : t -> int64
val int32_of_rpc : t -> int32
val int_of_rpc : t -> int
val bool_of_rpc : t -> bool
val float_of_rpc : t -> float
val string_of_rpc : t -> string
val dateTime_of_rpc : t -> string
val t_of_rpc : t -> t
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

(** [struct_extend rpc1 rpc2] first checks that [rpc1] and [rpc2] are both
 *  dictionaries. If this is the case then [struct_extend] will create a new
 *  [Rpc.t] which contains all key-value pairs from [rpc1], as well as all
 *  key-value pairs from [rpc2] for which the key does not exist in [rpc1]. *)
val struct_extend : t -> t -> t


