exception
  Parse_error of
    { offset : int
    ; message : string
    ; input : string
    }

type t = Rpc.t

val t_of_sexp : Sexplib0.Sexp.t -> t
val sexp_of_t : t -> Sexplib0.Sexp.t
val to_string : t -> string
val of_string : string -> t
val string_of_call : Rpc.call -> string
val call_of_string : string -> Rpc.call
val string_of_response : Rpc.response -> string
val response_of_string : string -> Rpc.response
