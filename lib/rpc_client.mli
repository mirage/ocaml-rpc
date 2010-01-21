(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

type content_type = [`XML | `JSON]

val string_of_content_type : content_type -> string
val content_type_of_string : string -> content_type

val do_rpc : content_type:content_type -> host:string -> port:int -> path:string -> Rpc.call -> Rpc.response

val do_rpc_unix : content_type:content_type -> filename:string -> path:string -> Rpc.call -> Rpc.response
