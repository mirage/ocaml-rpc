Small libraries to deal with RPCs in ocaml.

The syntax is quite simple:

type t = ... with rpc

will give two functions:
* val rpc_of_t : t -> Rpc.t
* val t_of_rpc : Rpc.t -> t

Then, the library support (currently) two protocols: XMLRPC and JSON(RPC). Functions signatures are:
* val Xmlrpc.to_string : Rpc.t -> string
* val Xmlrpc.of_string : string -> Rpc.t
* val Jsonrpc.to_string : Rpc.t -> string
* val Jsonrpc.of_string : string -> Rpc.t

So basically, if you want to marshal a value x of type t to JSON, you can use the following function:
Jsonrpc.to_string (rpc_of_t x)
