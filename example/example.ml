(* Example RPC *)
open Idl

(* No ppx required *)

(* APIs are declared as a functor which takes as an argument a module that conforms to the 'RPC' signature,
   defined in idl.ml
*)

module MyAPI(R : RPC) = struct
  open R

  (* We can define some named parameters here. These can be used in different RPC declarations *)
  let i1 = Param.mk ~name:"i1" ~description:"Parameter i1" Rpc.Types.int
  let s1 = Param.mk ~name:"s1" ~description:"Parameter s1" Rpc.Types.string
  let b1 = Param.mk Rpc.Types.bool

  let api1 = declare "api1" "Description of API 1" (i1 @-> s1 @-> returning b1)
  let api2 = declare "api2" "Description of API 2" (s1 @-> returning i1)

end

(* We can generate Client and Server modules *)
module Client=MyAPI(GenClient)
module Server=MyAPI(GenServer)


let _ =
  let open Rpc.Monad in

  (* The server is used by associating the RPC declarations with their implementations: *)
  let funcs =
    GenServer.empty ()
    |> Server.api1 (fun i s -> Printf.printf "Received '%d' and '%s': returning '%b'\n" i s true; true)
    |> Server.api2 (fun s -> Printf.printf "Received '%s': returning '%d'\n%!" s 56; 56)
  in

  (* The Server then has a 'server' function that can be used to service RPC requests *)
  let rpc_fn : Rpc.call -> Rpc.response Rpc.Monad.error_or = GenServer.server funcs in

  (* Wrap this in something to print out the marshalled call and response *)
  let rpc rpc =
    Printf.printf "Marshalled RPC call: '%s'\n" (Rpc.string_of_call rpc);
    rpc_fn rpc >>= fun response ->
    Printf.printf "Marshalled RPC type: '%s'\n" (Rpc.string_of_response response);
    Result.Ok response
  in

  (* This RPC function can actually be passed directly into the client for a short-circuit
     RPC *)
  Client.api1 rpc 7 "hello" >>= fun b -> Printf.printf "Result: %b\n" b;
  Client.api2 rpc "foo" >>= fun i -> Printf.printf "Result: %d\n" i;

  Result.Ok ()


(* More complex types can be constructed. For example, a record: *)

type vm = {
  name_label : string;
  name_description : string;
}

(* The fields are created *)
open Rpc.Types
       
let vm_name_label : (string, vm) field = { fname="name_label"; fdescription=""; field=Basic String }
let vm_name_description : (string, vm) field = { fname="name_description"; fdescription=""; field=Basic String }

(* And a 'structure' type *)
let vm : vm structure = { sname="vm"; fields = [ BoxedField vm_name_label; BoxedField vm_name_description ] }

(* vm <-> vm structure *)
open Rpcmarshal
open Rpc
open Rpc.Monad

let vm_of_vm_structure vm_struct =
  getf vm_name_label vm_struct >>= fun name_label ->
  getf vm_name_description vm_struct >>= fun name_description ->
  Result.Ok { name_label; name_description }
let vm_structure_of_vm vm =
  let vm_structure = { vfields = [] } in
  let vm_structure = setf vm_name_label vm.name_label vm_structure in
  let vm_structure = setf vm_name_description vm.name_description vm_structure in
  vm_structure

(* tydesc *)
let typ_of_vm = Struct vm
let vm_def = { name="vm"; description="VM record"; ty=typ_of_vm }


(* Or we can create a variant type *)
             
(* The tags are created first *)
type exnt
let errors : (string, exnt) tag = { vname="errors"; vdescription="Errors raised during an RPC invocation"; vcontents=Basic String }

(* And then we can create the 'variant' type *)
let exnt : exnt variant = { variants = [ BoxedTag errors ] }
let exnt_def = { name="exnt"; description="A variant type"; ty=Variant exnt }                        


(* This can then be used in RPCs *)

let p = Param.mk ~name:"vm" ~description:"Example structure" vm_def
let u = Param.mk Types.unit

module VMRPC (R : RPC) = struct
  open R

  let interface = describe Idl.Interface.({name="Vm"; description="My VM API"; version=1})

  let start = declare "start" "Start a VM"
      (p @-> returning u)
end

module VMClient = VMRPC(GenClient)
module VMServer = VMRPC(GenServer)


let _ =
  let open Rpc in
  let impl vm =
    match vm_of_vm_structure vm with
    | Result.Ok vm' -> 
      Printf.printf "name=%s description=%s\n" vm'.name_label vm'.name_description;
      ()
    | Result.Error x ->
      Printf.printf "Error with the VM!";
      ()
  in
  
  let funcs = GenServer.empty () |> VMServer.start impl in

  let rpc rpc =
    Printf.printf "Marshalled RPC call: '%s'\n" (string_of_call rpc);
    GenServer.server funcs rpc >>= fun response ->
    Printf.printf "Marshalled RPC type: '%s'\n" (string_of_response response);
    Result.Ok response
  in
  
  let test () = 
    let vm = vm_structure_of_vm { name_label="test"; name_description="description" } in
    VMClient.start rpc vm
  in
  test ()

(* Let's create some python *)

module Code=VMRPC(Codegen)
    
(* Generate some python *)
let _ =
  let interface = Code.(interface |> start) in
  let open Codegen.Interfaces in
  let interfaces = 
    empty "interfaces" "The test VM RPC interface"
      "This is an example of the ocaml-rpc code generator"
  in
  let interfaces = add_interface interfaces interface in
  let interfaces = register_exn interfaces exnt_def in
  Pythongen.of_interfaces (add_interface interfaces interface) |> Pythongen.string_of_ts |> Printf.printf "%s\n"

   
