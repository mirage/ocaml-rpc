(* Example RPC *)

(* The following is an example of how to use the ocaml-rpc library to define an
   interface, then use that interface to implement a client and server.

   The library is designed to be used with a ppx helper, but there is no
   requirement to use it, and for the sake of clearly describing the library
   this example does not use the ppx.
*)

(* Interfaces are described as a collection of methods which take some typed
   arguments and return a typed result, or possiby an error. These are declared
   as a functor which takes as an argument a module that conforms to the 'RPC'
   signature,defined in idl.ml.

   For example, the following functor declares 2 methods, `api1` and `api2`.
   The parameters are described first and can be fairly minimal (see `b1` for
   example)
*)

module MyAPI (R : Idl.RPC) = struct
  open R
  open Idl

  let description =
    Interface.
      { name = "MyAPI"
      ; namespace = Some "MYAPI"
      ; description =
          [ "This is an example showing how to use the IDL part of the ocaml-rpc "
          ; "library. What goes here is a description of the interface we're "
          ; "currently describing."
          ]
      ; version = 1, 0, 0
      }


  let implementation = implement description

  (* We can define some named parameters here. These can be used in different
     method declarations *)
  let i1 = Param.mk ~name:"i1" ~description:[ "Parameter i1" ] Rpc.Types.int
  let s1 = Param.mk ~name:"s1" ~description:[ "Parameter s1" ] Rpc.Types.string

  (* Parameters don't _have_ to have names and descriptions, in which case they
     will inherit from the name and description of the type. *)
  let b1 = Param.mk Rpc.Types.bool

  (* For the following methods, we use the default error type for any errors
     the methods may throw *)
  let e1 = Idl.DefaultError.err

  (* `declare` is defined in the RPC module passed in, and can do several
     different things. It is only the following two lines that actually do
     anything in this module - the declarations of parameters above are useful
     only in allowing the two declarations here to be succinct. *)
  let api1 = declare "api1" [ "Description 1" ] (i1 @-> s1 @-> returning b1 e1)
  let api2 = declare "api2" [ "Description 2" ] (s1 @-> returning i1 e1)
end

(* You can swap the rpc engine, by using a different monad here, 
   note however that if you are using an asynchronous one, like
   lwt or async, you should also use their specific IO functions
   including the print functions. *)
module M = Idl.IdM (* You can easily put ExnM here and the code would stay unchanged *)

module MyIdl = Idl.Make (M)

(* By passing in different modules to the `MyAPI` functor above, we can
   generate Client and Server modules *)
module Client = MyAPI (MyIdl.GenClient ())
module Server = MyAPI (MyIdl.GenServer ())

let _ =
  (* The Client module generated above makes use of the Result.t type,
     and hence it is convenient to use the Monadic `bind` and `return`
     functions in Rresult.R *)
  let open MyIdl in
  (* The server is used by associating the RPC declarations with their
     implementations. The return type is expected to be Result.t, hence
     the use of `ok` here. *)
  Server.api1 (fun i s ->
      Printf.printf "Received '%d' and '%s': returning '%b'\n" i s true;
      ErrM.return true);
  Server.api2 (fun s ->
      Printf.printf "Received '%s': returning '%d'\n%!" s 56;
      ErrM.return 56);
  (* The Server module has a 'server' function that can be used to service RPC
     requests by passing the funcs value created above. *)
  let rpc_fn = server Server.implementation in
  (* To see a little more clearly what's going on, we will wrap this rpc
     function in something to print out the marshalled call and response. *)
  let rpc rpc =
    let open M in
    Printf.printf "Marshalled RPC call: '%s'\n" (Rpc.string_of_call rpc);
    rpc_fn rpc
    >>= fun res ->
    Printf.printf "Marshalled RPC type: '%s'\n" (Rpc.string_of_response res)
    |> return
    >>= fun () ->
    (* You can actually use a semicolon here *)
    return res
  in
  (* The Client module exposes client-side implementations of the methods,
     which expect to be given an RPC function with a similar type;

        client_rpc: Rpc.call -> Rpc.response

     This function is intended to take the Rpc.call value, marshal it to
     some wire format and send it to the server. Once the server sends back
     the response, the client_rpc function unmarshals that from the wire
     format, and returns a value of typ Rpc.response.

     As it happens, we can pass the server rpc function directly to the client
     module as a short-circuit of the above process.
  *)
  let open ErrM in
  Client.api1 rpc 7 "hello"
  >>= fun b ->
  Printf.printf "Result: %b\n" b;
  Client.api2 rpc "foo"
  >>= fun i ->
  Printf.printf "Result: %d\n" i;
  return (Result.Ok ())


(* The above was using the built-in convenience types. More complex types can
   be constructed. For example, a record: *)

type vm =
  { name_label : string
  ; name_description : string
  }

(* In order to be able to marshal/unmarshal values of type 'vm', we need to
   do a bit of work. First we create values representing the fields: *)
open Rpc.Types

(* The field is described as an (`a, `b) field - `a is the type of the field
   and `b is the type of the record to which it belongs. Name and description
   are obvious. The `version` field is described elsewhere, the `type` field
   needs to match the `b of the declaration. `fget` and `fset` are the lens
   functions. *)
let vm_name_label : (string, vm) field =
  { fname = "name_label"
  ; fdescription = [ "The name of the VM." ]
  ; fversion = None
  ; fdefault = None
  ; field = Basic String
  ; fget = (fun f -> f.name_label)
  ; fset = (fun v s -> { s with name_label = v })
  }


let vm_name_description : (string, vm) field =
  { fname = "name_description"
  ; fdescription = [ "The description of the VM." ]
  ; fversion = None
  ; fdefault = None
  ; field = Basic String
  ; fget = (fun f -> f.name_description)
  ; fset = (fun v s -> { s with name_description = v })
  }


(* The constructor function here takes a 'field getter' argument. This is
   a record with a single field, `g`, with signature:

       g: 'a. string -> 'a Types.typ -> ('a, Rresult.R.msg) Result.t

   this is used to obtain the values for each field. With this mechanism,
   the details of how each field value is obtained is up to the caller of the
   constuctor
*)
let constructor getter =
  let open Rresult.R in
  getter.field_get "name_label" (Basic String)
  >>= fun name_label ->
  getter.field_get "name_description" (Basic String)
  >>= fun name_description -> return { name_label; name_description }


(* These values are combined to define a value of type `vm structure` here *)
let vm_structure : vm structure =
  { sname = "vm"
  ; version = None
  ; fields = [ BoxedField vm_name_label; BoxedField vm_name_description ]
  ; constructor
  }


let typ_of_vm = Struct vm_structure

(* Finally we create a value of type `vm structure def`, which names and
   describes the type *)
let vm =
  { name = "vm"
  ; description =
      [ "This record contains the static properties of a VM object,"
      ; "such as the name, description and other useful bits and pieces."
      ; "Runtime properties are part of some different record."
      ]
  ; ty = typ_of_vm
  }


(* Or we can create a variant type in a very similar way. As an example,
   we define a type `exnt` here: *)
type exnt = Errors of string

(* And create values representing the tags first (only one for this type
   definition). Once again, `name`, `description`, `version` and `contents` are
   similar to those defined for the fields, and `preview` and `review` are
   the prism functions (similar to the lens functions for the fields above). *)
let errors : (string, exnt) Rpc.Types.tag =
  Rpc.Types.
    { tname = "errors"
    ; tdescription = [ "Errors raised during an RPC invocation" ]
    ; tversion = None
    ; tcontents = Basic String
    ; tpreview =
        (function
        | Errors s -> Some s)
    ; treview = (fun s -> Errors s)
    }


(* And then we can create the 'variant' type *)
let exnt_variant : exnt variant =
  Rpc.Types.
    { vname = "exnt"
    ; variants = [ BoxedTag errors ]
    ; vversion = None
    ; vdefault = Some (Errors "unknown error tag!")
    ; vconstructor =
        (fun s t ->
          match String.lowercase_ascii s with
          | "errors" -> Rresult.R.map errors.treview (t.tget (Basic String))
          | s -> Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s))
    }


(* And finally we name and describe the type in an `exnt variant def` type *)
let exnt =
  { name = "exnt"; description = [ "A variant type" ]; ty = Variant exnt_variant }


(* If we want to use this as an error to any RPC call, we need to declare an
   exception and wrap this in an Idl.Error.t record *)
exception ExampleExn of exnt

let error =
  Idl.Error.
    { def = exnt
    ; raiser = (fun e -> ExampleExn e)
    ; matcher =
        (fun e ->
          match e with
          | ExampleExn x -> Some x
          | _ -> None)
    }


(* Note that all of the above can be automatically derived using the
   ppx_deriving_rpcty ppx. See later examples for descriptions of these *)

(* These new types can then be used in RPCs *)
let p = Idl.Param.mk ~name:"vm" ~description:[ "Example structure" ] vm

let b =
  Idl.Param.mk
    ~name:"paused"
    ~description:[ "Start the VM in a paused state" ]
    Rpc.Types.bool


let u = Idl.Param.mk Rpc.Types.unit
let err = error

module VMRPC (R : Idl.RPC) = struct
  open R

  (* We can declare some more information about the interface here for more
     interesting uses of these declarations - for example, the documentation
     generator or Cmdliner term generator *)
  let implementation =
    implement
      Idl.Interface.
        { name = "VM"
        ; namespace = None
        ; description =
            [ "The VM interface is used to perform power-state operations on virtual"
            ; "machines. It doesn't do anything else in this implementation as it is"
            ; "purely being used as an example of how to declare an interface."
            ]
        ; version = 1, 0, 0
        }


  let start =
    declare
      "start"
      [ "Start a VM. This method should be idempotent, and you can start the "
      ; "same VM as many times as you like."
      ]
      (p @-> b @-> returning u err)
end

(* Once again we generate a client and server module *)
module VMClient = VMRPC (MyIdl.GenClient ())
module VMServer = VMRPC (MyIdl.GenServer ())
module VMServerExn = VMRPC (Idl.Exn.GenServer ())

let _ =
  (* As before, we define an implementation of the method, and associate it
     with the server impls *)
  let impl vm' paused =
    let open MyIdl.ErrM in
    Printf.printf
      "name=%s description=%s paused=%b\n"
      vm'.name_label
      vm'.name_description
      paused;
    if paused then return_err (Errors "Paused start is unimplemented") else return ()
  in
  VMServer.start impl;
  (* And an implementation that raises exceptions rather than Result.t
     types *)
  let implexn _vm' paused =
    if paused then raise (ExampleExn (Errors "Paused start is unimplemented"));
    ()
  in
  VMServerExn.start implexn;
  (* Again we create a wrapper RPC function that dumps the marshalled data to
     stdout for clarity *)
  let rpcfn = MyIdl.server VMServer.implementation in
  let rpcfnexn = Idl.Exn.server VMServerExn.implementation in
  let rpc rpc =
    let open M in
    Printf.printf "Marshalled RPC call:\n'%s'\n" (Rpc.string_of_call rpc);
    rpcfn rpc
    >>= fun res ->
    Printf.printf "Marshalled RPC type:\n'%s'\n" (Rpc.string_of_response res)
    |> return
    >>= fun () ->
    (* You could have used a semicolon here *)
    return res
  in
  let rpcexn rpc =
    Printf.printf "Marshalled RPC call:\n'%s'\n" (Rpc.string_of_call rpc);
    let response = rpcfnexn rpc in
    Printf.printf
      "Marshalled RPC type from exception producing impl (should be the same):\n'%s'\n"
      (Rpc.string_of_response response);
    response
  in
  let module VMClientExn =
    VMRPC (Idl.Exn.GenClient (struct
      let rpc = rpcexn
    end))
  in
  (* And once again, we use a pass the server's rpc function as a
     short-circuit to the client. *)
  let test () =
    let vm = { name_label = "test"; name_description = "description" } in
    let open MyIdl in
    let client =
      let open M in
      VMClient.start (fun c -> rpc c) vm true
      |> T.get
      >>= function
      | Ok () -> Printf.printf "Unexpected OK\n%!" |> return
      | Error (Errors e) -> Printf.printf "Caught an (expected) error: %s\n%!" e |> return
    in
    let clientexn () =
      try
        VMClientExn.start vm true;
        Printf.printf "Unexpected OK!\n%!"
      with
      | ExampleExn (Errors s) -> Printf.printf "Caught an (expected) error: %s\n%!" s
    in
    M.run client;
    clientexn ()
  in
  test ()


(* We can use the generators to generate other pieces of information: *)
module C = VMRPC (Codegen.Gen ())

(* Here we use the HTML and Markdown generators to create descriptions of the
   interfaces *)
let _ =
  let interfaces =
    Codegen.Interfaces.empty
      "VM"
      "VM power-state interface"
      [ "This interface is used to demonstrate the declaration of an interface"
      ; "by an example of manipulating the power-states of VMs. It shows how"
      ; "new datatypes can be declared and used in methods, and how errors are"
      ; "handled."
      ]
    |> Codegen.Interfaces.add_interface (C.implementation ())
  in
  let write fname str =
    let oc = open_out fname in
    Printf.fprintf oc "%s" str;
    close_out oc
  in
  Markdowngen.to_string interfaces |> write "vm.md"
