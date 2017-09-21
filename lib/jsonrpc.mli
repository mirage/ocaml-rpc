type version = V1 | V2

val to_buffer : Rpc.t -> Buffer.t -> unit
val to_string : Rpc.t -> string
val to_fct : Rpc.t -> (string -> unit) -> unit [@@ocaml.deprecated]
val to_a : empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.t -> 'a [@@ocaml.deprecated]
val new_id : unit -> int64
val string_of_call: ?version:version -> Rpc.call -> string
val json_of_response : ?id:Rpc.t -> version -> Rpc.response -> Rpc.t
val json_of_error_object : ?data:Rpc.t option -> int64 -> string -> Rpc.t
val string_of_response: ?id:Rpc.t -> ?version:version -> Rpc.response -> string
val a_of_response : ?id:Rpc.t -> ?version:version -> empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.response -> 'a [@@ocaml.deprecated]
val of_string : string -> Rpc.t
val of_fct : (unit -> char) -> Rpc.t [@@ocaml.deprecated]
val of_a : next_char:('a -> char) -> 'a -> Rpc.t [@@ocaml.deprecated]
exception Malformed_method_request of string
exception Malformed_method_response of string
val get : string -> (string * 'a) list -> 'a
val call_of_string : string -> Rpc.call
val version_id_and_call_of_string : string -> version * Rpc.t * Rpc.call
val response_of_stream : (unit -> char) -> Rpc.response [@@ocaml.deprecated]
val response_of_string : string -> Rpc.response
val response_of_in_channel : in_channel -> Rpc.response
