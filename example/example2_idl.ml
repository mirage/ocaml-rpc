(* Example2 *)

open Rpc
open Idl

let sockpath = "/tmp/rpcsock"

module Datatypes = struct
  module Query = struct
    type t = {
      name: string;
      vendor: string;
      version: string;
      features: string list;
      instance_id: string;
    } [@@deriving rpcty]
  end

  type err = | InternalError of string | FrobnicationFailed | OperationInProgress [@@deriving rpcty]
end

module API(R : RPC) = struct
  open R

  let query_p = Param.mk ~name:"query" ~description:"Parameter i1" Datatypes.Query.t
  let unit_p = Param.mk Types.unit
  let string_p = Param.mk Types.string

  let domid_p = Param.mk ~name:"domid" ~description:"Domain ID" Types.int64
  let vm_p = Param.mk ~name:"vm" ~description:"VM uuid" Types.string
  let vm_description = Param.mk ~name:"description" ~description:"Description" Types.string

  let err = Datatypes.err

  let query = declare
      "query"
      "Query the details of the server"
      (unit_p @-> returning query_p err)

  let diagnostics = declare
      "get_diagnostics"
      "Get diagnostics information from the server"
      (unit_p @-> returning string_p err)

  let test = declare
      "test"
      "A test of a bit more of the IDL stuff"
      (domid_p @-> vm_p @-> vm_description @-> returning query_p err)
end
