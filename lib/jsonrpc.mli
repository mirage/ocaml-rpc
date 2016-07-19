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

val to_string : Rpc.t -> string
val of_string : string -> Rpc.t

val to_fct : Rpc.t -> (string -> unit) -> unit
val of_fct : (unit -> char) -> Rpc.t

val to_a : empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.t -> 'a
val of_a : next_char:('a  -> char) -> 'a -> Rpc.t

type version = V1 | V2

val string_of_call: ?version:version -> Rpc.call -> string
val call_of_string: string -> Rpc.call

val string_of_response: ?version:version -> Rpc.response -> string
val response_of_string: string -> Rpc.response
val response_of_in_channel : in_channel -> Rpc.response

val a_of_response : ?version:version -> empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.response -> 'a
