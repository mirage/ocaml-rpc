(* Example RPC *)

(* The following is an example of how to use the ocaml-rpc library to define an
   interface, then use that interface to implement a client and server.

   The library is designed to be used with a ppx helper, but there is no
   requirement to use it, and for the sake of clearly describing the library
   this example does not use the ppx.
*)

open Idl

(* Interfaces are described as a collection of methods which take some typed
   arguments and return a typed result, or possiby an error. These are declared
   as a functor which takes as an argument a module that conforms to the 'RPC'
   signature,defined in idl.ml.

   For example, the following functor declares 2 methods, `api1` and `api2`.
   The parameters are described first and can be fairly minimal (see `b1` for
   example)
*)

module MyAPI(R : RPC) = struct
  open R

  (* We can define some named parameters here. These can be used in different
     method declarations *)
  let i1 = Param.mk ~name:"i1" ~description:"Parameter i1" Rpc.Types.int
  let s1 = Param.mk ~name:"s1" ~description:"Parameter s1" Rpc.Types.string

  (* Parameters don't _have_ to have names and descriptions, in which case they
     will inherit from the name and description of the type. *)
  let b1 = Param.mk Rpc.Types.bool

  (* For the following methods, we use the default error type for any errors
     the methods may throw *)
  let e1 = Idl.DefaultError.def

  (* `declare` is defined in the RPC module passed in, and can do several
     different things. It is only the following two lines that actually do
     anything in this module - the declarations of parameters above are useful
     only in allowing the two declarations here to be succinct. *)
  let api1 = declare "api1" "Description 1" (i1 @-> s1 @-> returning b1 e1)
  let api2 = declare "api2" "Description 2" (s1 @-> returning i1 e1)

end

(* By passing in different modules to the `MyAPI` functor above, we can
   generate Client and Server modules *)
module Client=MyAPI(GenClient)
module Server=MyAPI(GenServer)

let _ =
  (* The Client module generated above makes use of the Result.result type,
     and hence it is convenient to use the Monadic `bind` and `return`
     functions in Rresult.R *)
  let open Rresult.R in

  (* The server is used by associating the RPC declarations with their
     implementations. The return type is expected to be Result.result, hence
     the use of `ok` here. *)
  let funcs =
    GenServer.empty ()

    |> Server.api1 (fun i s ->
        Printf.printf "Received '%d' and '%s': returning '%b'\n" i s true;
        ok true)

    |> Server.api2 (fun s ->
        Printf.printf "Received '%s': returning '%d'\n%!" s 56;
        ok 56)
  in

  (* The Server module has a 'server' function that can be used to service RPC
     requests by passing the funcs value created above. *)
  let rpc_fn : Rpc.call -> Rpc.response = GenServer.server funcs in

  (* To see a little more clearly what's going on, we will wrap this rpc
     function in something to print out the marshalled call and response. *)
  let rpc rpc =
    Printf.printf "Marshalled RPC call: '%s'\n"
      (Rpc.string_of_call rpc);
    let response = rpc_fn rpc in
    Printf.printf "Marshalled RPC type: '%s'\n"
      (Rpc.string_of_response response);
    response
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
  Client.api1 rpc 7 "hello" >>= fun b -> Printf.printf "Result: %b\n" b;
  Client.api2 rpc "foo" >>= fun i -> Printf.printf "Result: %d\n" i;

  Result.Ok ()


(* The above was using the built-in convenience types. More complex types can
   be constructed. For example, a record: *)

type vm = {
  name_label : string;
  name_description : string;
}

(* In order to be able to marshal/unmarshal values of type 'vm', we need to
   do a bit of work. First we create values representing the fields: *)
open Rpc.Types

(* The field is described as an (`a, `b) field - `a is the type of the field
   and `b is the type of the record to which it belongs. Name and description
   are obvious. The `version` field is described elsewhere, the `type` field
   needs to match the `b of the declaration. `fget` and `fset` are the lens
   functions. *)
let vm_name_label : (string, vm) field = {
  fname="name_label";
  fdescription="";
  fversion=None;
  field=Basic String;
  fget = (fun f -> f.name_label);
  fset = (fun v s -> {s with name_label = v})
}
let vm_name_description : (string, vm) field = {
  fname="name_description";
  fdescription="";
  fversion=None;
  field=Basic String;
  fget = (fun f -> f.name_description);
  fset = (fun v s -> {s with name_description = v})
}

(* The constructor function here takes a 'field getter' argument. This is
   a record with a single field, `g`, with signature:

       g: 'a. string -> 'a Types.typ -> ('a, Rresult.R.msg) Result.result

   this is used to obtain the values for each field. With this mechanism,
   the details of how each field value is obtained is up to the caller of the
   constuctor
*)
let constructor getter =
  let open Rresult.R in
  getter.g "name_label" (Basic String) >>= fun name_label ->
  getter.g "name_description" (Basic String) >>= fun name_description ->
  return { name_label; name_description }

(* And finally we define a value of type `vm structure` here that uses the
   values defined above. *)
let vm : vm structure = {
  sname="vm";
  fields = [ BoxedField vm_name_label; BoxedField vm_name_description ];
  constructor;
}

(* tydesc *)
let typ_of_vm = Struct vm
let vm_def = { name="vm"; description="VM record"; ty=typ_of_vm }


(* Or we can create a variant type *)

(* The tags are created first *)
type exnt = | Errors of string
let errors : (string, exnt) Rpc.Types.tag = Rpc.Types.{
    vname = "errors";
    vdescription = "Errors raised during an RPC invocation";
    vversion = None;
    vcontents = Basic String;
    vpreview = (function (Errors s) -> Some s);
    vreview = (fun s -> Errors s)
  }

(* And then we can create the 'variant' type *)
let exnt : exnt variant = Rpc.Types.{
  variants = [ BoxedTag errors ];
  vconstructor = (fun s t ->
      match s with
      | "Errors" -> Rresult.R.map errors.vreview (t.t (Basic String))
      | s -> Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s))
  }
let exnt_def = { name="exnt"; description="A variant type"; ty=Variant exnt }


(* This can then be used in RPCs *)

let p = Param.mk ~name:"vm" ~description:"Example structure" vm_def
let err = exnt_def
let u = Param.mk Rpc.Types.unit

module VMRPC (R : RPC) = struct
  open R

  let interface = describe Idl.Interface.({name="Vm"; description="My VM API"; version=(1,0,0)})

  let start = declare "start" "Start a VM"
      (p @-> returning u err)
end

module VMClient = VMRPC(GenClient)
module VMServer = VMRPC(GenServer)


let _ =
  let open Rresult.R in
  let impl vm' =
    Printf.printf "name=%s description=%s\n" vm'.name_label vm'.name_description; ok ()
  in

  let funcs = GenServer.empty () |> VMServer.start impl in

  let rpc rpc =
    Printf.printf "Marshalled RPC call: '%s'\n" (Rpc.string_of_call rpc);
    let response = GenServer.server funcs rpc in
    Printf.printf "Marshalled RPC type: '%s'\n" (Rpc.string_of_response response);
    response
  in

  let test () =
    let vm = { name_label="test"; name_description="description" } in
    VMClient.start rpc vm
  in
  test ()
