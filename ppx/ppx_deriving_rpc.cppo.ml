open Longident
open Asttypes
open Parsetree
open Location
open Ast_helper
open Ast_convenience

let deriver = "rpc"

module Of_rpc = struct
  let str_of_type ~options ~path type_decl =
    let typ_of_lid = Ppx_deriving.mangle_type_decl (`Prefix "of_typ") type_decl in
    [%expr fun rpc ->
      match Rpcmarshal.unmarshal [%e str typ_of_lid] rpc with
      | Result.Ok value -> value
      | Error (`Msg message) ->
        failwith (Printf.sprintf "Error unmarshalling %s: %s" type_decl.ptype_name.txt message) 
    ]
end

module Rpc_of = struct
  let str_of_type ~options ~path type_decl =
    let typ_of_lid = Ppx_deriving.mangle_type_decl (`Prefix "typ_of") type_decl in
    [%expr fun value ->
      Rpcmarshal.marshal [%e str typ_of_lid] value
    ]
end

let rpc_strs_of_type ~options ~path type_decl =
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let rpc_of = Ppx_deriving.mangle_type_decl ~fixpoint:"" (`Prefix "rpc_of") type_decl in
  let of_rpc = Ppx_deriving.mangle_type_decl ~fixpoint:"" (`Suffix "of_rpc") type_decl in
  List.concat [
    Ppx_deriving_rpcty.rpcty_strs_of_type ~options ~path type_decl;
    [
    Vb.mk (pvar rpc_of)
      (Exp.fun_ Label.nolabel None (pvar ("__x__")) [%expr [%e (polymorphize (Rpc_of.str_of_type ~options ~path type_decl))] __x__]);
    Vb.mk (pvar of_rpc)
      (Exp.fun_ Label.nolabel None (pvar ("__x__")) [%expr [%e (polymorphize (Of_rpc.str_of_type ~options ~path type_decl))] __x__]);
    ]
  ]

let () =
  let open Ppx_deriving in
  register
    (create deriver
       (* ~core_type: (Rpc_of.expr_of_typ) *)
       ~type_decl_str:(fun ~options ~path type_decls ->
           [Str.value Recursive
              (List.concat (List.map (rpc_strs_of_type ~options ~path) type_decls))])
       ());
