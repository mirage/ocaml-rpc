`ocaml-rpc` is a libraries to deal with RPCs in ocaml.

# RPC types

An RPC value is defined as follow:

    type t =
      | Int of int64
      | Bool of bool
      | Float of float
      | String of string
      | DateTime of string
      | Enum of t list
      | Dict of (string * t) list
      | Null


# Generating code

The idea behind `ocaml-rpc` is to generate functions to convert values of a
given type to and from theirs RPC representations.

In order to do so, it is sufficicient to add `with rpc` to the
corresponding type defintion. Hence :

    type t = ... [@@deriving rpc]

this will give two functions:

* A function to convert values of type `t` to values of type `Rpc.t` :
  `val rpc_of_t : t -> Rpc.t`

* A function to convert values of type `Rpc.t` to values of type `t` :
  `val t_of_rpc : Rpc.t -> (t,string) Result.result`

Optionaly, it is possible to have different field name in the OCaml
type (if it is a record) and in the dictionary argumenent (the first
elements of `Dict`):

    type t = { foo: int [@key "type"]; bar: int [@key "let"]; } [@@deriving rpc]

This will replace "foo" by "type" and "bar" by "let" in the RPC 
representation. This is particularly useful when you want to integrate
with an existing API and the field names are not valid OCaml identifiers.

# Conversion functions

`rpc` currently support two protocols: XMLRPC and JSON(RPC). Functions signatures are:

    val Xmlrpc.to_string : Rpc.t -> string
    val Xmlrpc.of_string : string -> Rpc.t
    val Jsonrpc.to_string : Rpc.t -> string
    val Jsonrpc.of_string : string -> Rpc.t

So if you want to marshal a value x of type t to JSON, you can use the following function:

    Jsonrpc.to_string (rpc_of_t x)

# Dependencies

* [xmlm](http://erratique.ch/software/xmlm)
* [ppx_deriving](http://github.com/whitequark/ppx_deriving)
* [type-conv](http://hg.ocaml.info/release/type-conv) (for deprecated camlp4 extension)
