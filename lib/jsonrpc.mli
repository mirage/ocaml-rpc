val list_iter_between : ('a -> unit) -> (unit -> 'b) -> 'a list -> unit
val escape_string : string -> string
val to_fct : Rpc.t -> (string -> unit) -> unit
val to_buffer : Rpc.t -> Buffer.t -> unit
val to_string : Rpc.t -> string
val to_a : empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.t -> 'a
val new_id : unit -> int64
val string_of_call : Rpc.call -> string
val json_of_response : Rpc.response -> Rpc.t
val string_of_response : Rpc.response -> string
val a_of_response :
  empty:(unit -> 'a) -> append:('a -> string -> unit) -> Rpc.response -> 'a
type error =
    Unexpected_char of int * char * string
  | Invalid_value of int * string * string
  | Invalid_leading_zero of int * string
  | Unterminated_value of int * string
  | Internal_error of int * string
exception Parse_error of error
val of_fct : (unit -> char) -> Rpc.t
val of_string : string -> Rpc.t
val of_a : next_char:('a -> char) -> 'a -> Rpc.t
exception Malformed_method_request of string
exception Malformed_method_response of string
val get : string -> (string * 'a) list -> 'a
val call_of_string : string -> Rpc.call
val response_of_stream : (unit -> char) -> Rpc.response
val response_of_string : string -> Rpc.response
val response_of_in_channel : in_channel -> Rpc.response
