exception Parse_error of string * string * Xmlm.input
val pretty_string_of_error : string -> string -> Xmlm.input -> string
val parse_error : string -> string -> Xmlm.input -> unit

val encode : string -> string

val to_string : Rpc.t -> string
val string_of_call : Rpc.call -> string
val string_of_response : Rpc.response -> string

val of_string : ?callback:(string list -> Rpc.t -> unit) -> string -> Rpc.t

val call_of_string :
  ?callback:(string list -> Rpc.t -> unit) -> string -> Rpc.call

val response_of_fault :
  ?callback:(string list -> Rpc.t -> unit) -> Xmlm.input -> Rpc.response
val response_of_success :
  ?callback:(string list -> Rpc.t -> unit) -> Xmlm.input -> Rpc.response
val response_of_input :
  ?callback:(string list -> Rpc.t -> unit) -> Xmlm.input -> Rpc.response
val response_of_string :
  ?callback:(string list -> Rpc.t -> unit) -> string -> Rpc.response
val response_of_in_channel :
  ?callback:(string list -> Rpc.t -> unit) -> in_channel -> Rpc.response
