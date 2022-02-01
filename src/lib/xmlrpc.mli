val encode : string -> string
val to_string : ?strict:bool -> Rpc.t -> string

val to_a
  :  ?strict:bool
  -> empty:(unit -> 'a)
  -> append:('a -> string -> unit)
  -> Rpc.t
  -> 'a
  [@@ocaml.deprecated]

val string_of_call : ?strict:bool -> Rpc.call -> string
val string_of_response : ?strict:bool -> Rpc.response -> string

val a_of_response
  :  ?strict:bool
  -> empty:(unit -> 'a)
  -> append:('a -> string -> unit)
  -> Rpc.response
  -> 'a
  [@@ocaml.deprecated]

exception Parse_error of string * string * Xmlm.input

(** The parsing functions make it possible to specify the routine used to
    decode base64 values. The default is to use [Base64.decode_exn] which
    strictly interprets the standard. A different function will be required if
    the XMLRPC server inserts line breaks into the base64 encoding (as per
    RFC2045). *)

val pretty_string_of_error : string -> string -> Xmlm.input -> string
val parse_error : string -> string -> Xmlm.input -> unit
val of_string
  : ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> string -> Rpc.t

val of_a
  :  ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> next_char:('b -> char option)
  -> 'b
  -> Rpc.t
  [@@ocaml.deprecated]

val call_of_string
  : ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> string -> Rpc.call

val response_of_fault
  :  ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> Xmlm.input
  -> Rpc.response

val response_of_success
  :  ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> Xmlm.input
  -> Rpc.response

val response_of_input
  :  ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> Xmlm.input
  -> Rpc.response

val response_of_string
  :  ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> string
  -> Rpc.response

val response_of_in_channel
  :  ?callback:(string list -> Rpc.t -> unit)
  -> ?base64_decoder:(string -> string)
  -> in_channel
  -> Rpc.response
