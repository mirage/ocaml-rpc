
open Longident
open Asttypes
open Parsetree
open Location
open Ast_helper
open Ast_convenience

let deriver = "rpcty"

let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> x
  | None   -> default

(* This is for renaming fields where there's a keyword clash with ocaml *)
let attr_key  = attr_string "key"

(* This is for naming variants where there's a keyword clash with ocaml *)
let attr_name  = attr_string "name"

(* Documentation for variants / record members *)
let attr_doc = attr_string "doc"

let attr_default attrs =
  Ppx_deriving.attr ~deriver "default" attrs |> Ppx_deriving.Arg.(get_attr ~deriver expr)

let argn = Printf.sprintf "a%d"

(* For these types we have convertors in rpc.ml *)
let core_types = List.map (fun (s, y) -> (Lident s, y))
    ["unit", [%expr Unit];
     "int", [%expr Basic Int];
     "int32", [%expr Basic Int32];
     "int64", [%expr Basic Int64];
     "string", [%expr Basic String];
     "float", [%expr Basic Float];
     "bool", [%expr Basic Bool]]

module Typ_of = struct

  let wrap_runtime decls =
    [%expr let open! Rpc.Types in [%e decls]]

  let rec expr_of_typ  typ =
    match typ with
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } when
        List.mem_assoc lid core_types -> List.assoc lid core_types
    | { ptyp_desc = Ptyp_constr ( { txt = Lident "char" }, args ) } ->
      [%expr Basic Char]
    | [%type: (string * [%t? typ]) list] ->
      [%expr Dict (String, [%e expr_of_typ typ])]
    | [%type: [%t? typ] list] ->
      [%expr List [%e expr_of_typ  typ]]
    | [%type: [%t? typ] array] ->
      [%expr Array [%e expr_of_typ  typ]]
    | {ptyp_desc = Ptyp_tuple typs } ->
      List.fold_right (fun t acc -> [%expr Tuple ([%e expr_of_typ  t], [%e acc])]) (List.tl typs) [%expr [%e (expr_of_typ  (List.hd typs))] ]
    | [%type: [%t? typ] option] ->
      [%expr Option [%e expr_of_typ  typ]]
    | { ptyp_desc = Ptyp_constr ( { txt = lid }, args ) } ->
      [%expr [%e Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "typ_of") lid))]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let mk n t d = [%expr BoxedTag ([%e record ["vname", n; "vcontents", t; "vdescription", d]])] in
      let cases =
        fields |> List.map (fun field ->
            match field with
            | Rtag (label, attrs, true, []) ->
              mk (str (attr_key label attrs)) [%expr Unit] (str (attr_doc "" attrs))
            | Rtag (label, attrs, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
              mk (str (attr_key label attrs)) [%expr Tuple [%e list (List.map (expr_of_typ ) typs)]] (str (attr_doc "" attrs))
            | Rtag (label, attrs, false, [typ]) ->
              mk (str (attr_key label attrs)) (expr_of_typ  typ) (str (attr_doc "" attrs))
            | _ ->
              raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                deriver (Ppx_deriving.string_of_core_type typ))
      in
      [%expr Variant ({ variants=[%e list cases]; })]
    | { ptyp_desc = Ptyp_any } ->
      failwith "Ptyp_any not handled"
    | { ptyp_desc = Ptyp_var name } ->
      [%expr [%e evar ("poly_"^name)]]
    | { ptyp_desc = Ptyp_poly (_, _) } ->
      failwith "Ptyp_poly not handled"
    | { ptyp_desc = Ptyp_extension _ } ->
      failwith "Ptyp_extension not handled"
    | { ptyp_desc = Ptyp_arrow (_, _, _) } ->
      failwith "Ptyp_arrow not handled"
    | { ptyp_desc = Ptyp_object (_, _) } ->
      failwith "Ptyp_object not handled"
    | { ptyp_desc = Ptyp_alias (_, _) } ->
      failwith "Ptyp_alias not handled"
    | { ptyp_desc = Ptyp_class (_, _) } ->
      failwith "Ptyp_class not handled"
    | { ptyp_desc = Ptyp_package _ } ->
      failwith "Ptyp_package not handled"
  (*  | _ -> failwith "Error"*)

  let str_of_type ~options ~path type_decl =
    let name = type_decl.ptype_name.txt in
    let mytype = Ppx_deriving.core_type_of_type_decl type_decl in
    let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
    let typ_of_lid = Ppx_deriving.mangle_type_decl (`Prefix "typ_of") type_decl in
    let param_of_lid = Ppx_deriving.mangle_type_decl (`Suffix "def") type_decl in
    let typ_of =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest ->
        [ Vb.mk (pvar typ_of_lid) (polymorphize (expr_of_typ manifest))]
      | Ptype_record labels, _ ->
        let fields =
          labels |> List.map (fun { pld_name = { txt = fname }; pld_type; pld_attributes } ->
              let rpc_name = attr_name fname pld_attributes in
              let default = attr_default pld_attributes in
              let field_name = String.concat "_" [name; fname] in
              let fget = [%expr fun _r -> [%e Exp.field (evar "_r") (mknoloc (Lident fname)) ] ] in
              let fset = [%expr fun v s -> [%e record [fname, [%expr v]] ~over:([%expr s])]] in
              (fname,
               field_name,
               pld_type,
               [%expr let open Rpc.Types in
                 [%e record ["fname", str rpc_name;
                             "field", expr_of_typ pld_type;
                             "fdescription", str (attr_doc "" pld_attributes);
                             "fget", fget;
                             "fset", fset;
                            ] ] ], default ))
        in
        let field_name_bindings = List.map (fun (fname, field_name, typ, record, _) ->
            Vb.mk (Pat.constraint_ (pvar field_name)
                     ([%type: (_, [%t mytype]) Rpc.Types.field]))
              record) fields in
        let boxed_fields = list (List.map (fun (_,field_name,_,_,_) ->
            [%expr BoxedField ([%e Exp.ident (lid field_name)])]) fields) in
        let construct_record = List.fold_left (fun expr (fname,field_name,pld_type,_,def) ->
            match def with
            | Some d ->
              [%expr getter.Rpc.Types.g (*~default:[%e d]*) [%e str field_name] [%e expr_of_typ pld_type] >>= fun [%p pvar field_name] -> [%e expr]]
            | None ->
              [%expr getter.Rpc.Types.g [%e str field_name] [%e expr_of_typ pld_type] >>= fun [%p pvar field_name] -> [%e expr]]
          )
            [%expr return [%e Exp.record (List.map (fun (fname, field_name, _, _, _) ->
                mknoloc (Lident fname), evar field_name) fields) None]]
            fields in
        field_name_bindings @
        [ Vb.mk (pvar name)
            ([%expr let open Rpc.Types in ({ fields=[%e boxed_fields ]; sname=[%e str name]; constructor = fun getter -> let open Rpc.Monad in [%e construct_record] }
                    : [%t mytype ] Rpc.Types.structure) ] ) ] @
        [ Vb.mk (pvar typ_of_lid)
            (polymorphize
               ([%expr Rpc.Types.Struct [%e Exp.ident (lid name) ]])) ]
      | Ptype_abstract, None ->
        failwith "Unhandled"
      | Ptype_open, _ ->
        failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_attributes } ->
              let rpc_name = attr_key name pcd_attributes in
              let contents = match pcd_args with
                | [] -> [%expr Unit]
                | _ -> List.fold_right (fun t acc ->
                    [%expr Tuple ([%e expr_of_typ  t], [%e acc])])
                    (List.tl pcd_args)
                    [%expr [%e (expr_of_typ  (List.hd pcd_args))]]
              in
              let args = List.mapi (fun i typ -> evar (argn i)) pcd_args in
              let pattern = List.mapi (fun i _ -> pvar (argn i)) pcd_args in
              let vpreview = Exp.function_ [
                  Exp.case (pconstr name pattern) [%expr Some [%e tuple args ]];
                  Exp.case (Pat.any ()) [%expr None]
                ]
              in
              let vreview = Exp.function_ [Exp.case (ptuple pattern) (constr name args)] in
              let variant = [%expr BoxedTag [%e record ["vname", str rpc_name; "vcontents", contents; "vdescription", str (attr_doc "" pcd_attributes); "vpreview", vpreview; "vreview", vreview]]] in
              let vconstructor_case = Exp.case (Pat.constant (Const_string (name,None))) [%expr Rpc.Monad.bind (t.t [%e contents]) ([%e Exp.function_ [Exp.case (ptuple pattern) [%expr Rpc.Monad.return [%e (constr name args)]]]])] in
              (variant, vconstructor_case))
        in
        let default = if List.length cases = 1 then [] else [Exp.case (Pat.any ()) [%expr Rpc.Monad.error_of_string (Printf.sprintf "Unknown tag '%s'" s)]] in
        let vconstructor = [%expr fun s t -> [%e Exp.match_ (evar "s") ((List.map snd cases) @ default)]] in
        [ Vb.mk (pvar typ_of_lid) (polymorphize ([%expr Variant ({ variants=([%e list (List.map fst cases)]); vconstructor=[%e vconstructor] } : [%t mytype ] variant) ])) ]
    in
    let doc = attr_doc "" type_decl.ptype_attributes in
    let name = type_decl.ptype_name.txt in
    typ_of @ [Vb.mk (pvar param_of_lid) (wrap_runtime (record ["name", str name; "description", str doc; "ty", Exp.ident (lid typ_of_lid)]))]

end

let rpcty_strs_of_type ~options ~path type_decl =
  Typ_of.str_of_type ~options ~path type_decl


let () =
  let open Ppx_deriving in
  register
    (create deriver
       ~type_decl_str:(fun ~options ~path type_decls ->
           [Str.value Recursive
              (List.concat (List.map (rpcty_strs_of_type ~options ~path) type_decls))])
       ());
