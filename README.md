# OCaml-RPC -- remote procedure calls (RPC) library

`ocaml-rpc` is a library that provides remote procedure calls (RPC)
using XML or JSON as transport encodings. The transport mechanism itself
is outside the scope of this library as all conversions are from and to
strings. The `odoc` generated documentation is available at
[mirage.github.io/ocaml-rpc/rpclib](http://mirage.github.io/ocaml-rpc/rpclib/index.html).

[![Build Status](https://travis-ci.org/mirage/ocaml-rpc.svg?branch=master)](https://travis-ci.org/mirage/ocaml-rpc)

## RPC types

An RPC value is defined as follow:

```ocaml
type t =
    Int of int64
  | Int32 of int32
  | Bool of bool
  | Float of float
  | String of string
  | DateTime of string
  | Enum of t list
  | Dict of (string * t) list
  | Base64 of string
  | Null
```

## Generating code

The idea behind `ocaml-rpc` is to generate type definitions that can be used to
convert values of a given type to and from their RPC representations.

In order to do so, it is sufficient to add `[@@deriving rpcty]` to the
corresponding type definition. Hence :

```ocaml
type t = ... [@@deriving rpcty]
```

This will give a value `typ_of_t` of type `Rpc.Types.typ`, which can be used in
conjunction with the `Rpcmarshal` module to:

* Convert values of type `t` to values of type `Rpc.t`:
  ```ocaml
  let rpc_of_t t = Rpcmarshal.marshal typ_of_t t
  ```

* Convert values of type `Rpc.t` to values of type `t` :
  ```ocaml
  let t_of_rpc rpc = Rpcmarshal.unmarshal typ_of_t rpc
  ```

Optionally, it is possible to have different field name in the OCaml
type (if it is a record) and in the dictionary argument (the first
elements of `Dict`):

```ocaml
type t = { foo: int [@key "type"]; bar: int [@key "let"]; } [@@deriving rpcty]
```

This will replace "foo" by "type" and "bar" by "let" in the RPC
representation. This is particularly useful when you want to integrate
with an existing API and the field names are not valid OCaml identifiers.

The library also provides the `[@@deriving rpc]` ppx, which is similar to
`rpcty`, but directly generates the conversion functions.

```ocaml
type t = ... [@@deriving rpc]
```

will give two functions:

* A function to convert values of type `t` to values of type `Rpc.t` :
  `val rpc_of_t : t -> Rpc.t`

* A function to convert values of type `Rpc.t` to values of type `t` :
  `val t_of_rpc : Rpc.t -> (t,string) Result.t`

It also supports the `@key` annotations for having different field names:
```ocaml
type t = { foo: int [@key "type"]; bar: int [@key "let"]; } [@@deriving rpc]
```

## Conversion functions

`ocaml-rpc` currently support two protocols: XMLRPC and JSON(RPC). Function
signatures are:

```ocaml
val Xmlrpc.to_string : Rpc.t -> string
val Xmlrpc.of_string : string -> Rpc.t
val Jsonrpc.to_string : Rpc.t -> string
val Jsonrpc.of_string : string -> Rpc.t
```

So if you want to marshal a value x of type t to JSON, you can use the
following function:

```ocaml
Jsonrpc.to_string (rpc_of_t x)
```

## IDL generator

The `Idl` module makes it possible to define an abstract interface in OCaml
using the following pattern:

```ocaml
module CalcInterface(R : Idl.RPC) = struct
  open R

  let int_p = Idl.Param.mk Rpc.Types.int

  let add = R.declare "add"
      ["Add two numbers"]
      (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let mul = R.declare "mul"
      ["Multiply two numbers"]
      (int_p @-> int_p @-> returning int_p Idl.DefaultError.err)

  let implementation = implement
      { Idl.Interface.name = "Calc"; namespace = Some "Calc"; description = ["Calculator supporting addition and multiplication"]; version = (1,0,0) }
end
```

Then we can generate various "bindings" from it by passing a module
implementing the `RPC` signature to this functor:

* OCaml bindings for clients or servers can be generated using one of the
  `GenClient*` or `GenServer*` functors, respectively.

  For example one can generate an RPC client this way:
  ```ocaml
  module CalcClient :
    sig
      val add :
        (Rpc.call -> Rpc.response) ->
        int -> int -> (int, Idl.DefaultError.t) result
      val mul :
        (Rpc.call -> Rpc.response) ->
        int -> int -> (int, Idl.DefaultError.t) result
    end = CalcInterface(Idl.GenClient ())
  ```
  The functions in the resulting `CalcClient` module can be used to call their
  corresponding RPC methods.
  `CalcClient` does not implement the transport mechanism itself, that should
  be provided by passing an a rpc function of type `Rpc.call -> Rpc.response`.

  `CalcClient.add rpc 4 5` will marshal the parameters `4` and `5` into their
  RPC representations, construct an `Rpc.call`, pass that call to the given `rpc`
  function, and return either an `Ok` containing the unmarshalled result or an
  `Error` with the error description depending on the response returned by
  `rpc`.

  There are variations of the `GenClient` module:

  `GenClientExn` raises an exception in case the response indicates a failure,
  instead of returning a `result`:
  ```ocaml
  module CalcClient :
    sig
      val add : (Rpc.call -> Rpc.response) -> int -> int -> int
      val mul : (Rpc.call -> Rpc.response) -> int -> int -> int
    end = CalcInterface(Idl.GenClientExn ())
  ```

  and `GenClientExnRpc` allows one to specify the rpc function once when
  constructing the client module:
  ```ocaml
  module CalcClient :
    sig
      val add : int -> int -> int
      val mul : int -> int -> int
    end = CalcInterface(Idl.GenClientExnRpc (struct let rpc = rpc end))
  ```

  Bindings for a server can be generated in a similar way:
  ```ocaml
  module CalcServer :
    sig
      val add : (int -> int -> (int, Idl.DefaultError.t) result) -> unit
      val mul : (int -> int -> (int, Idl.DefaultError.t) result) -> unit
      val implementation : Idl.server_implementation
    end = CalcInterface(Idl.GenServer ())
  ```
  The implementations of each RPC method should be specified by passing it to
  the corresponding function in `CalcServer`:
  ```ocaml
  CalcServer.add (fun a b -> Ok (a + b));
  CalcServer.mul (fun a b -> Ok (a * b));
  ```
  Then we can generate our server from the `implementation` (in case of
  `GenClient`, `implementation` is unused):
  ```ocaml
  let rpc : (Rpc.call -> Rpc.response) = Idl.server CalcServer.implementation
  ```
  Again, the transport mechanism is not implemented by `CalcServer`. We just
  get an rpc function that, given an `Rpc.call`, calls the implementation of
  that RPC method and performs the marshalling and unmarshalling.
  It is up to the user of this library to connect this `rpc` function to a real
  server that responds to client requests.

  Here we also have a `GenServerExn` functor, for server implementations that
  raise exceptions instead of returning a `result`.

  The `rpclib-lwt` and `rpclib-async` packages provide similar client and
  server generators that use `Lwt` and `Async`, respectively.

  The `Xmlrpc` and `Jsonrpc` modules can be helpful when implementing the `rpc`
  function for an XML-RPC or JSON-RPC client/server: they provide functions for
  converting rpc requests and responses to/from their respective wire formats.

* Commandline interfaces can be generated using `Cmdlinergen`:
  ```ocaml
  module CalcCli :
    sig
      val implementation :
        unit ->
        ((Rpc.call -> Rpc.response) ->
         (unit -> unit) Cmdliner.Term.t * Cmdliner.Term.info)
        list
    end = CalcInterface(Cmdlinergen.Gen ())
  ```
  We can use the `implementation` to construct the CLI.
  Again, we need to pass an `rpc` function that knows how to make RPC calls.
  ```ocaml
  let () =
    let cmds = (List.map (fun t -> t rpc) (CalcCli.implementation ())) in
    let open Cmdliner in
    Term.(exit @@ eval_choice default_cmd cmds)
  ```

* Some generators use the output of `Codegen`. This functor generates a structure
  that contains information about the methods, their parameters, return types,
  etc. Currently these generators that use the output of `Codegen` require the
  method parameters to be named.

  ```ocaml
  module CalcInterface(R : Idl.RPC) = struct
    open R

    let int_p_1 = Idl.Param.mk ~name:"int1" Rpc.Types.int
    let int_p_2 = Idl.Param.mk ~name:"int2" Rpc.Types.int
    let int_p = Idl.Param.mk Rpc.Types.int

    let add = R.declare "add"
        ["Add two numbers"]
        (int_p_1 @-> int_p_2 @-> returning int_p Idl.DefaultError.err)

    let implementation = implement
        { Idl.Interface.name = "Calc"; namespace = Some "Calc"; description = ["Calculator supporting addition and multiplication"]; version = (1,0,0) }
  end

  module CalcCode :
    sig
      val implementation : unit -> Codegen.Interface.t
    end = CalcInterface(Codegen.Gen ())

  let interfaces = Codegen.Interfaces.create
    ~name:"calc"
    ~title:"Calculator"
    ~description:["Interface for a Calculator"]
    ~interfaces:[CalcCode.implementation ()]
  ```

  * `Markdowngen` can generate a markdown file documenting these interfaces:
    ```ocaml
    let md = Markdowngen.to_string interfaces
    ```
  * `Pythongen` can generate Python code that contains various classes
    wrapping a Python implementation, providing typechecking & method
    dispatch, and a CLI.
    ```ocaml
    let code = Pythongen.of_interfaces interfaces |> Pythongen.string_of_ts
    ```

The possibilities are not limited to the above generators provided by
`ocaml-rpc`. Any third-party module implementing the `RPC` signature can be
used to generate something from an interface defined following the above
pattern. For example, it is possible to write an `RPC` implementation that
generates a GUI for a given interface.

## Base64 Decoding

The treatment of line feeds (and other characters) in 
[XML-RPC](http://xmlrpc.com) base64-encoded data is underspecified.

By default, this library decodes values using the `Base64.decode_exn` 
function of [ocaml-base64](https://github.com/mirage/ocaml-base64). This 
function implements [RFC4648](https://datatracker.ietf.org/doc/html/rfc4648) 
which requires the rejection of non-alphabet characters for security reasons 
(see [section 3.3](https://datatracker.ietf.org/doc/html/rfc4648#section-3.3)
and also [section 3.1](https://datatracker.ietf.org/doc/html/rfc4648#section-3.1)).

This is problematic when communicating with servers that are less strict. 
For instance, the 
[encode](https://docs.python.org/3/library/xmlrpc.client.html#xmlrpc.client.Binary.encode) 
function of the Python `xmlrpc.client` refers to [section 6.8 of 
RFC2045](https://datatracker.ietf.org/doc/html/rfc2045.html#section-6.8) to 
justify inserting a newline character every 76 characters.
For this reasons, the functions in `Xmlrpc` allow the caller to override the 
`base64_decoder`. The following declaration gives a rough-and-ready 
“dangerous” implementation based on the `Base64.rfc2045` package.
A better implementation would only accept a `\n` every 76 characters.

```
let base64_2045_decoder s =
  let open Base64_rfc2045 in
  let buf = Buffer.create 1024 in
  let d = decoder (`String s) in
  let rec go () =
    match decode d with
    | `Flush s -> (Buffer.add_string buf s; go ())
    | `End -> Buffer.contents buf
    (* best-effort *)
    | `Malformed _   (* ignore missing '\r' before '\n', etc. *)
    | `Wrong_padding (* ignore *)
    | `Await -> go ()
  in
  go ()
```

## Building

To build, first install the dependencies:

```sh
opam install dune base64 ppxlib async js_of_ocaml-ppx lwt cow cmdliner rresult yojson xmlm
```
For tests:

```sh
opam install alcotest alcotest-lwt
```
