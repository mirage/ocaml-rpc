open Idl
open Rpc
open Types
let dbg =
  Param.mk ~name:"dbg" ~description:"Debug context from the caller"
    Types.string
let unit = Param.mk Types.unit
let task_id = Param.mk ~name:"task_id" Types.string
type uri = string[@@deriving rpcty][@@doc
                                     "A URI representing the means for accessing the volume data. The interpretation of the URI is specific to the implementation. Xapi will choose which implementation to use based on the URI scheme."]
let rec typ_of_uri = Basic String
and uri_def =
  let open! Rpc.Types in
    {
      name = "uri";
      description =
        "A URI representing the means for accessing the volume data. The interpretation of the URI is specific to the implementation. Xapi will choose which implementation to use based on the URI scheme.";
      ty = typ_of_uri
    }
type blocklist =
  {
  blocksize: int[@doc "size of the individual blocks"];
  ranges:
    (int64* int64) list[@doc
                         "list of block ranges, where a range is a (start,length) pair, measured in units of [blocksize]"];}
[@@deriving rpcty][@@doc "List of blocks for copying"]
let rec (blocklist_blocksize :(_,blocklist) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "blocksize";
      field = (Basic Int);
      fdescription = "size of the individual blocks"
    }
and (blocklist_ranges :(_,blocklist) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "ranges";
      field = (List (Tuple ((Basic Int64), (Basic Int64))));
      fdescription =
        "list of block ranges, where a range is a (start,length) pair, measured in units of [blocksize]"
    }
and blocklist =
  let open Rpc.Types in
    ({
       fields = [BoxedField blocklist_blocksize; BoxedField blocklist_ranges];
       sname = "blocklist"
     } : blocklist Rpc.Types.structure)
and typ_of_blocklist = Rpc.Types.Struct blocklist
and blocklist_of_structure _r =
  let open Rpc.Monad in
    (Rpcmarshal.getf blocklist_ranges _r) >>=
      (fun blocklist_ranges  ->
         (Rpcmarshal.getf blocklist_blocksize _r) >>=
           (fun blocklist_blocksize  ->
              return
                { blocksize = blocklist_blocksize; ranges = blocklist_ranges
                }))
and structure_of_blocklist _r =
  Rpcmarshal.setf blocklist_ranges _r.ranges
    (Rpcmarshal.setf blocklist_blocksize _r.blocksize
       { Rpc.Types.vfields = [] })
and blocklist_def =
  let open! Rpc.Types in
    {
      name = "blocklist";
      description = "List of blocks for copying";
      ty = typ_of_blocklist
    }
type exn =
  | Unimplemented of string[@@deriving rpcty]
let rec typ_of_exn =
  Variant
    ({
       variants =
         [BoxedTag
            {
              vname = "Unimplemented";
              vcontents = (Basic String);
              vdescription = ""
            }]
     } : exn variant)
and exn_def =
  let open! Rpc.Types in { name = "exn"; description = ""; ty = typ_of_exn }
type domain = string[@@deriving rpcty][@@doc
                                        "A string representing a Xen domain on the local host. The string is guaranteed to be unique per-domain but it is not guaranteed to take any particular form. It may (for example) be a Xen domain id, a Xen VM uuid or a Xenstore path or anything else chosen by the toolstack. Implementations should not assume the string has any meaning."]
let rec typ_of_domain = Basic String
and domain_def =
  let open! Rpc.Types in
    {
      name = "domain";
      description =
        "A string representing a Xen domain on the local host. The string is guaranteed to be unique per-domain but it is not guaranteed to take any particular form. It may (for example) be a Xen domain id, a Xen VM uuid or a Xenstore path or anything else chosen by the toolstack. Implementations should not assume the string has any meaning.";
      ty = typ_of_domain
    }
type implementation =
  | Blkback of string[@doc "use kernel blkback with the given 'params' key"]
  | Qdisk of
  string[@doc "use userspace qemu qdisk with the given 'params' key"]
  | Tapdisk3 of
  string[@doc "use userspace tapdisk3 with the given 'params' key"][@@deriving
                                                                    rpcty]
[@@doc "The choice of blkback to use."]
let rec typ_of_implementation =
  Variant
    ({
       variants =
         [BoxedTag
            {
              vname = "Blkback";
              vcontents = (Basic String);
              vdescription = "use kernel blkback with the given 'params' key"
            };
         BoxedTag
           {
             vname = "Qdisk";
             vcontents = (Basic String);
             vdescription =
               "use userspace qemu qdisk with the given 'params' key"
           };
         BoxedTag
           {
             vname = "Tapdisk3";
             vcontents = (Basic String);
             vdescription =
               "use userspace tapdisk3 with the given 'params' key"
           }]
     } : implementation variant)
and implementation_def =
  let open! Rpc.Types in
    {
      name = "implementation";
      description = "The choice of blkback to use.";
      ty = typ_of_implementation
    }
type backend =
  {
  domain_uuid: string[@doc "UUID of the domain hosting the backend"];
  implementation: implementation[@doc "choice of implementation technology"];}
[@@deriving rpcty][@@doc
                    "A description of which Xen block backend to use. The toolstack needs this to setup the shared memory connection to blkfront in the VM."]
let rec (backend_domain_uuid :(_,backend) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "domain_uuid";
      field = (Basic String);
      fdescription = "UUID of the domain hosting the backend"
    }
and (backend_implementation :(_,backend) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "implementation";
      field = typ_of_implementation;
      fdescription = "choice of implementation technology"
    }
and backend =
  let open Rpc.Types in
    ({
       fields =
         [BoxedField backend_domain_uuid; BoxedField backend_implementation];
       sname = "backend"
     } : backend Rpc.Types.structure)
and typ_of_backend = Rpc.Types.Struct backend
and backend_of_structure _r =
  let open Rpc.Monad in
    (Rpcmarshal.getf backend_implementation _r) >>=
      (fun backend_implementation  ->
         (Rpcmarshal.getf backend_domain_uuid _r) >>=
           (fun backend_domain_uuid  ->
              return
                {
                  domain_uuid = backend_domain_uuid;
                  implementation = backend_implementation
                }))
and structure_of_backend _r =
  Rpcmarshal.setf backend_implementation _r.implementation
    (Rpcmarshal.setf backend_domain_uuid _r.domain_uuid
       { Rpc.Types.vfields = [] })
and backend_def =
  let open! Rpc.Types in
    {
      name = "backend";
      description =
        "A description of which Xen block backend to use. The toolstack needs this to setup the shared memory connection to blkfront in the VM.";
      ty = typ_of_backend
    }
type persistent = bool[@@deriving rpcty][@@doc
                                          "True means the disk data is persistent and should be preserved when the datapath is closed i.e. when a VM is shutdown or rebooted. False means the data should be thrown away when the VM is shutdown or rebooted."]
let rec typ_of_persistent = Basic Bool
and persistent_def =
  let open! Rpc.Types in
    {
      name = "persistent";
      description =
        "True means the disk data is persistent and should be preserved when the datapath is closed i.e. when a VM is shutdown or rebooted. False means the data should be thrown away when the VM is shutdown or rebooted.";
      ty = typ_of_persistent
    }
let uri =
  Param.mk
    ~description:"A URI which represents how to access the volume disk data."
    Common.uri_def
let persistent = Param.mk persistent_def
let domain =
  Param.mk ~description:"An opaque string which represents the Xen domain."
    domain_def
let backend = Param.mk backend_def
open Idl
module Datapath(R:RPC) =
  struct
    open R
    let interface =
      R.describe
        {
          Idl.Interface.name = "Datapath";
          description =
            "Xapi will call the functions here on VM start/shutdown/suspend/resume/migrate. Every function is idempotent. Every function takes a domain parameter which allows the implementation to track how many domains are currently using the volume.";
          version = 1
        }
    let open_ =
      declare "open"
        "[open uri persistent] is called before a disk is attached to a VM. If persistent is true then care should be taken to persist all writes to the disk. If persistent is false then the implementation should configure a temporary location for writes so they can be thrown away on [close]."
        (uri @-> (persistent @-> (returning unit)))
    let attach =
      declare "attach"
        "[attach uri domain] prepares a connection between the storage named by [uri] and the Xen domain with id [domain]. The return value is the information needed by the Xen toolstack to setup the shared-memory blkfront protocol. Note that the same volume may be simultaneously attached to multiple hosts for example over a migrate. If an implementation needs to perform an explicit handover, then it should implement [activate] and [deactivate]. This function is idempotent."
        (uri @-> (domain @-> (returning backend)))
    let activate =
      declare "activate"
        "[activate uri domain] is called just before a VM needs to read or write its disk. This is an opportunity for an implementation which needs to perform an explicit volume handover to do it. This function is called in the migration downtime window so delays here will be noticeable to users and should be minimised. This function is idempotent."
        (uri @-> (domain @-> (returning unit)))
    let deactivate =
      declare "deactivate"
        "[deactivate uri domain] is called as soon as a VM has finished reading or writing its disk. This is an opportunity for an implementation which needs to perform an explicit volume handover to do it. This function is called in the migration downtime window so delays here will be noticeable to users and should be minimised. This function is idempotent."
        (uri @-> (domain @-> (returning unit)))
    let detach =
      declare "detach"
        "[detach uri domain] is called sometime after a VM has finished reading or writing its disk. This is an opportunity to clean up any resources associated with the disk. This function is called outside the migration downtime window so can be slow without affecting users. This function is idempotent. This function should never fail. If an implementation is unable to perform some cleanup right away then it should queue the action internally. Any error result represents a bug in the implementation."
        (uri @-> (domain @-> (returning unit)))
    let close =
      declare "close"
        "[close uri] is called after a disk is detached and a VM shutdown. This is an opportunity to throw away writes if the disk is not persistent."
        (uri @-> (returning unit))
  end
module Data(R:RPC) =
  struct
    open R
    let interface =
      describe
        (let open Idl.Interface in
           {
             name = "Data";
             description =
               "This interface is used for long-running data operations such as copying the contents of volumes or mirroring volumes to remote destinations";
             version = 1
           })
    type operation =
      | Copy of uri*
      uri[@doc
           "Copy (src,dst) represents an on-going copy operation from the [src] URI to the [dst] URI"]
      | Mirror of uri*
      uri[@doc
           "Mirror (src,dst) represents an on-going mirror operation from the [src] URI to the [dst] URI"]
    [@@deriving rpcty][@@doc
                        "The primary key for referring to a long-running operation"]
    let rec typ_of_operation =
      Variant
        ({
           variants =
             [BoxedTag
                {
                  vname = "Copy";
                  vcontents = (Tuple (typ_of_uri, typ_of_uri));
                  vdescription =
                    "Copy (src,dst) represents an on-going copy operation from the [src] URI to the [dst] URI"
                };
             BoxedTag
               {
                 vname = "Mirror";
                 vcontents = (Tuple (typ_of_uri, typ_of_uri));
                 vdescription =
                   "Mirror (src,dst) represents an on-going mirror operation from the [src] URI to the [dst] URI"
               }]
         } : operation variant)
    and operation_def =
      let open! Rpc.Types in
        {
          name = "operation";
          description =
            "The primary key for referring to a long-running operation";
          ty = typ_of_operation
        }
    type operations = operation list[@@deriving rpcty][@@doc
                                                        "A list of operations"]
    let rec typ_of_operations = List typ_of_operation
    and operations_def =
      let open! Rpc.Types in
        {
          name = "operations";
          description = "A list of operations";
          ty = typ_of_operations
        }
    type status =
      {
      failed:
        bool[@doc
              "[failed] will be set to true if the operation has failed for some reason"];
      progress:
        float option[@doc
                      "[progress] will be returned for a copy operation, and ranges between 0 and 1"];}
    [@@deriving rpcty][@@doc "Status information for on-going tasks"]
    let rec (status_failed :(_,status) Rpc.Types.field)=
      let open Rpc.Types in
        {
          fname = "failed";
          field = (Basic Bool);
          fdescription =
            "[failed] will be set to true if the operation has failed for some reason"
        }
    and (status_progress :(_,status) Rpc.Types.field)=
      let open Rpc.Types in
        {
          fname = "progress";
          field = (Option (Basic Float));
          fdescription =
            "[progress] will be returned for a copy operation, and ranges between 0 and 1"
        }
    and status =
      let open Rpc.Types in
        ({
           fields = [BoxedField status_failed; BoxedField status_progress];
           sname = "status"
         } : status Rpc.Types.structure)
    and typ_of_status = Rpc.Types.Struct status
    and status_of_structure _r =
      let open Rpc.Monad in
        (Rpcmarshal.getf status_progress _r) >>=
          (fun status_progress  ->
             (Rpcmarshal.getf status_failed _r) >>=
               (fun status_failed  ->
                  return
                    { failed = status_failed; progress = status_progress }))
    and structure_of_status _r =
      Rpcmarshal.setf status_progress _r.progress
        (Rpcmarshal.setf status_failed _r.failed { Rpc.Types.vfields = [] })
    and status_def =
      let open! Rpc.Types in
        {
          name = "status";
          description = "Status information for on-going tasks";
          ty = typ_of_status
        }
    let remote =
      Param.mk ~name:"remote"
        ~description:"A URI which represents how to access a remote volume disk data."
        uri_def
    let operation = Param.mk operation_def
    let blocklist = Param.mk blocklist_def
    let copy =
      declare "copy"
        "[copy uri domain remote blocks] copies [blocks] from the local disk to a remote URI. This may be called as part of a Volume Mirroring operation, and hence may need to cooperate with whatever process is currently mirroring writes to ensure data integrity is maintained"
        (uri @->
           (domain @-> (remote @-> (blocklist @-> (returning operation)))))
    let mirror =
      declare "mirror"
        "[mirror uri domain remote] starts mirroring new writes to the volume to a remote URI (usually NBD). This is called as part of a volume mirroring process"
        (uri @-> (domain @-> (remote @-> (returning operation))))
    let status = Param.mk status_def
    let stat =
      declare "stat"
        "[stat operation] returns the current status of [operation]. For a copy operation, this will contain progress information."
        (operation @-> (returning status))
    let cancel =
      declare "cancel"
        "[cancel operation] cancels a long-running operation. Note that the call may return before the operation has finished."
        (operation @-> (returning unit))
    let destroy =
      declare "destroy"
        "[destroy operation] destroys the information about a long-running operation. This should fail when run against an operation that is still in progress."
        (operation @-> (returning unit))
    let operations = Param.mk operations_def
    let ls =
      declare "ls" "[ls] returns a list of all current operations"
        (unit @-> (returning operations))
  end
