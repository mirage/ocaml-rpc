open Base
open Ppxlib
open Ast_builder.Default
open Common

let deriver = "rpcty"

let typ_of_f s = match s with "t" -> "typ_of" | s -> "typ_of_" ^ s

module Typ_of = struct
  let rec analyse_variant_case ~loc is_polyvar tcls include_preview_default
      cname pcd_args version doc rpc_name =
    let module Ast_builder = (val Ast_builder.make loc) in
    let open Ast_builder in
    let argn = Printf.sprintf "a%d" in
    let lower_rpc_name = String.lowercase rpc_name in
    let typs =
      match pcd_args with
      | Pcstr_tuple typs -> typs
      | Pcstr_record _ ->
          Location.raise_errorf "%s: record variants are not supported" deriver
    in
    let contents =
      match typs with
      | [] -> [%expr Unit]
      | [t1] -> expr_of_typ ~loc t1
      | [t1; t2] ->
          [%expr Tuple ([%e expr_of_typ ~loc t1], [%e expr_of_typ ~loc t2])]
      | [t1; t2; t3] ->
          [%expr
            Tuple3
              ( [%e expr_of_typ ~loc t1]
              , [%e expr_of_typ ~loc t2]
              , [%e expr_of_typ ~loc t3] )]
      | [t1; t2; t3; t4] ->
          [%expr
            Tuple4
              ( [%e expr_of_typ ~loc t1]
              , [%e expr_of_typ ~loc t2]
              , [%e expr_of_typ ~loc t3]
              , [%e expr_of_typ ~loc t4] )]
      | _ ->
          Location.raise_errorf "%s: Tuples with arity > 4 are not supported"
            deriver
    in
    let args = List.mapi ~f:(fun i _ -> evar (argn i)) typs in
    let pattern = List.mapi ~f:(fun i _ -> pvar (argn i)) typs in
    let pat = ppat_tuple_opt pattern in
    let pat' = Option.value pat ~default:punit in
    let opt_args = pexp_tuple_opt args in
    let expr = Option.value opt_args ~default:eunit in
    let constr =
      if is_polyvar then pexp_variant cname opt_args
      else pexp_construct (Located.mk (lident cname)) opt_args
    in
    let vpreview_default =
      if include_preview_default then
        [case ~guard:None ~lhs:ppat_any ~rhs:[%expr None]]
      else []
    in
    let tag_pattern =
      if is_polyvar then ppat_variant cname pat
      else ppat_construct (Located.mk (lident cname)) pat
    in
    let vpreview =
      pexp_function
        ( [case ~lhs:tag_pattern ~guard:None ~rhs:[%expr Some [%e expr]]]
        @ vpreview_default )
    in
    let vreview = pexp_function [case ~lhs:pat' ~guard:None ~rhs:constr] in
    let variant =
      [%expr
        BoxedTag
          [%e
            pexp_record
              (List.map
                 ~f:(fun (x, y) ->
                   (Located.mk (Ldot (Ldot (Lident "Rpc", "Types"), x)), y) )
                 [ ("tname", estring rpc_name)
                 ; ("tcontents", contents)
                 ; ("tversion", version)
                 ; ("tcls", tcls)
                 ; ("tdescription", doc)
                 ; ("tpreview", vpreview)
                 ; ("treview", vreview) ])
              None]]
    in
    let vconstructor_case =
      case
        ~lhs:(ppat_constant (Pconst_string (lower_rpc_name, None)))
        ~guard:None
        ~rhs:
          [%expr
            Rresult.R.bind
              (t.tget [%e contents])
              [%e
                pexp_function
                  [ case ~lhs:pat' ~guard:None
                      ~rhs:[%expr Rresult.R.ok [%e constr]] ]]]
    in
    (variant, vconstructor_case)

  (** [expr_of_typ ~loc typ] returns an expression of type `Rpc.Types.typ` that 
      represents the  core_type typ`. *)
  and expr_of_typ ~loc typ =
    let expr =
      match typ with
      | {ptyp_desc= Ptyp_constr ({txt= Lident name; _}, _); _}
        when List.Assoc.mem (core_types loc) ~equal:String.equal name ->
          List.Assoc.find_exn (core_types loc) ~equal:String.equal name
      | {ptyp_desc= Ptyp_constr ({txt= Lident "char"; _}, _); _} ->
          [%expr Rpc.Types.(Basic Char)]
      | [%type: (string * [%t? typ]) list] ->
          [%expr Rpc.Types.Dict (Rpc.Types.String, [%e expr_of_typ ~loc typ])]
      | [%type: [%t? typ] list] ->
          [%expr Rpc.Types.List [%e expr_of_typ ~loc typ]]
      | [%type: [%t? typ] array] ->
          [%expr Rpc.Types.Array [%e expr_of_typ ~loc typ]]
      | {ptyp_desc= Ptyp_tuple [t1; t2]; _} ->
          [%expr
            Rpc.Types.Tuple ([%e expr_of_typ ~loc t1], [%e expr_of_typ ~loc t2])]
      | {ptyp_desc= Ptyp_tuple [t1; t2; t3]; _} ->
          [%expr
            Rpc.Types.Tuple3
              ( [%e expr_of_typ ~loc t1]
              , [%e expr_of_typ ~loc t2]
              , [%e expr_of_typ ~loc t3] )]
      | {ptyp_desc= Ptyp_tuple [t1; t2; t3; t4]; _} ->
          [%expr
            Rpc.Types.Tuple4
              ( [%e expr_of_typ ~loc t1]
              , [%e expr_of_typ ~loc t2]
              , [%e expr_of_typ ~loc t3]
              , [%e expr_of_typ ~loc t4] )]
      | {ptyp_desc= Ptyp_tuple _; _} ->
          failwith "Tuples with arity > 4 are not supported"
      | [%type: [%t? typ] option] ->
          [%expr Rpc.Types.Option [%e expr_of_typ ~loc typ]]
      | {ptyp_desc= Ptyp_constr (lid, _); _} ->
          type_constr_conv lid ~loc ~f:typ_of_f []
      | {ptyp_desc= Ptyp_variant (fields, _, _); _} ->
          let inherits, tags =
            List.partition_tf
              ~f:(function Rinherit _ -> true | _ -> false)
              fields
          in
          let bad = [%expr failwith "Unknown tag/contents"] in
          let tcls =
            pexp_construct ~loc
              (Located.mk ~loc (Ldot (Ldot (lident "Rpc", "Types"), "PolyType")))
              None
          in
          let default_expr =
            match Attribute.get Attrs.ct_default typ with
            | None -> bad
            | Some expr ->
                [%expr
                  match rpc' with
                  | String _ | Enum (String _ :: _) -> [%e expr]
                  | _ -> [%e bad]]
          in
          let tag_cases =
            tags
            |> List.map ~f:(fun field ->
                   match field with
                   | Rtag (label, _attrs, true, []) ->
                       let label' = String.lowercase label.txt in
                       let version =
                         expr_of_option ~loc
                         @@ Attribute.get Attrs.rtag_version field
                       in
                       let doc = [%expr []] in
                       let name =
                         match Attribute.get Attrs.rt_name field with
                         | Some s -> s
                         | None -> label'
                       in
                       analyse_variant_case ~loc true tcls true label'
                         (Pcstr_tuple []) version doc name
                   | Rtag
                       (label, _attrs, false, [{ptyp_desc= Ptyp_tuple typs; _}])
                     ->
                       let label' = String.lowercase label.txt in
                       let version =
                         expr_of_option ~loc
                         @@ Attribute.get Attrs.rtag_version field
                       in
                       let doc = [%expr []] in
                       let name =
                         match Attribute.get Attrs.rt_name field with
                         | Some s -> s
                         | None -> label'
                       in
                       analyse_variant_case ~loc true tcls true label'
                         (Pcstr_tuple typs) version doc name
                   (*            | Rtag (_label, _attrs, false, [_typ])*)
                   | _ -> failwith "Cannot derive variant case" )
          and _inherits_case =
            (*let toplevel_typ = typ in*)
            let expr =
              List.map
                ~f:(function Rinherit typ -> typ | _ -> assert false)
                inherits
              |> List.fold_left
                   ~f:(fun expr typ ->
                     [%expr
                       try
                         [%e expr_of_typ ~loc typ] rpc
                         (*  :> [%t toplevel_typ]*)
                       with _ -> [%e expr]] )
                   ~init:default_expr
            in
            case ~lhs:[%pat? _] ~guard:None ~rhs:expr
          in
          let default =
            [ case ~lhs:(ppat_any ~loc) ~guard:None
                ~rhs:
                  [%expr
                    Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s)]
            ]
          in
          let vconstructor =
            [%expr
              fun s' t ->
                let s = String.lowercase_ascii s' in
                [%e
                  pexp_match ~loc (evar ~loc "s")
                    (List.map ~f:snd tag_cases @ default)]]
          in
          [%expr
            Rpc.Types.Variant
              ( { Rpc.Types.vname= [%e estring ~loc "poly"]
                ; Rpc.Types.variants=
                    [%e elist ~loc (List.map ~f:fst tag_cases)]
                ; Rpc.Types.vdefault= None
                ; Rpc.Types.vversion= None
                ; Rpc.Types.vconstructor= [%e vconstructor] }
                : [%t typ] Rpc.Types.variant )]
      | {ptyp_desc= Ptyp_any; _} -> failwith "Ptyp_any not handled"
      | {ptyp_desc= Ptyp_var name; _} -> evar ~loc ("poly_" ^ name)
      | {ptyp_desc= Ptyp_poly (_, _); _} -> failwith "Ptyp_poly not handled"
      | {ptyp_desc= Ptyp_extension _; _} ->
          failwith "Ptyp_extension not handled"
      | {ptyp_desc= Ptyp_arrow (_, _, _); _} ->
          failwith "Ptyp_arrow not handled"
      | {ptyp_desc= Ptyp_object (_, _); _} ->
          failwith "Ptyp_object not handled"
      | {ptyp_desc= Ptyp_alias (_, _); _} -> failwith "Ptyp_alias not handled"
      | {ptyp_desc= Ptyp_class (_, _); _} -> failwith "Ptyp_class not handled"
      | {ptyp_desc= Ptyp_package _; _} -> failwith "Ptyp_package not handled"
    in
    expr

  type field_def =
    { f_name: string
    ; rpc_name: string
    ; value_name: string
    ; expr: expression
    ; default: expression option
    ; typ: expression }

  (** [str_of_type loc type_decl] returns a list of value bindings that are values of
      type `Rpc.Types.def` and `Rpc.Types.typ` that represent the newly defined type.
       *)
  let str_of_type loc type_decl =
    let module Ast_builder = (val Ast_builder.make loc) in
    let open Ast_builder in
    let name = type_decl.ptype_name.txt in
    let core_type = core_type_of_type_declaration type_decl in
    let polymorphize = poly_fun_of_type_decl ~loc type_decl in
    let typ_of_lid = typ_of_f name in
    let typ_of =
      match (type_decl.ptype_kind, type_decl.ptype_manifest) with
      | Ptype_abstract, Some manifest ->
          [ value_binding ~pat:(pvar typ_of_lid)
              ~expr:(polymorphize (expr_of_typ ~loc manifest)) ]
      | Ptype_record labels, _ ->
          let one_field = match labels with [_] -> true | _ -> false in
          let analyse_field label_declaration =
            let f_name = label_declaration.pld_name.txt in
            let {pld_type; pld_attributes; _} = label_declaration in
            let rpc_name =
              Option.value
                (Attribute.get Attrs.key label_declaration)
                ~default:f_name
            in
            let value_name = String.concat ~sep:"_" [name; f_name] in
            let default =
              Attribute.get Attrs.label_default label_declaration
            in
            let version =
              Attribute.get Attrs.label_version label_declaration
            in
            let typ =
              Option.value
                (Attribute.get Attrs.label_typ label_declaration)
                ~default:(expr_of_typ ~loc pld_type)
            in
            let doc =
              Common.get_doc ~loc
                (Attribute.get Attrs.label_doc label_declaration)
                pld_attributes
            in
            let fget =
              [%expr
                fun _r ->
                  [%e pexp_field (evar "_r") (Located.mk (Lident f_name))]]
            in
            let fset =
              [%expr
                fun v _s ->
                  [%e
                    pexp_record
                      [(Located.mk (lident f_name), [%expr v])]
                      (if one_field then None else Some [%expr _s])]]
            in
            let fcls =
              pexp_construct (Located.mk (lident (String.uppercase name))) None
            in
            let expr =
              pexp_record
                (List.map
                   ~f:(fun (x, y) ->
                     (Located.mk (Ldot (Ldot (Lident "Rpc", "Types"), x)), y)
                     )
                   [ ("fname", elist [estring rpc_name])
                   ; ("field", expr_of_typ ~loc pld_type)
                   ; ("fcls", fcls)
                   ; ("fdefault", expr_of_option ~loc default)
                   ; ("fdescription", doc)
                   ; ("fversion", expr_of_option ~loc version)
                   ; ("fget", fget)
                   ; ("fset", fset) ])
                None
            in
            {f_name; rpc_name; value_name; expr; default; typ}
          in
          let fields = List.map ~f:analyse_field labels in
          let field_name_bindings =
            List.map
              ~f:(fun fld ->
                value_binding
                  ~pat:
                    (ppat_constraint (pvar fld.value_name)
                       [%type: (_, [%t core_type]) Rpc.Types.field])
                  ~expr:fld.expr )
              fields
          in
          let boxed_fields =
            elist
              (List.map
                 ~f:(fun fld ->
                   [%expr
                     Rpc.Types.BoxedField
                       [%e pexp_ident (Located.mk (lident fld.value_name))]] )
                 fields)
          in
          let construct_record =
            List.fold_left
              ~f:(fun expr fld ->
                match fld.default with
                | Some d ->
                    [%expr
                      ( match
                          getter.Rpc.Types.field_get [%e estring fld.rpc_name]
                            [%e fld.typ]
                        with
                      | Result.Ok _ as y -> y
                      | Result.Error _ -> Result.Ok [%e d] )
                      >>= fun [%p pvar fld.value_name] -> [%e expr]]
                | None ->
                    [%expr
                      getter.Rpc.Types.field_get [%e estring fld.rpc_name]
                        [%e fld.typ]
                      >>= fun [%p pvar fld.value_name] -> [%e expr]] )
              ~init:
                [%expr
                  return
                    [%e
                      pexp_record
                        (List.map
                           ~f:(fun fld ->
                             ( Located.mk (lident fld.f_name)
                             , evar fld.value_name ) )
                           fields)
                        None]]
              fields
          in
          field_name_bindings
          @ [ value_binding ~pat:(pvar typ_of_lid)
                ~expr:
                  (polymorphize
                     [%expr
                       Rpc.Types.Struct
                         ( { Rpc.Types.fields= [%e boxed_fields]
                           ; Rpc.Types.sname= [%e estring name]
                           ; Rpc.Types.version=
                               [%e
                                 expr_of_option ~loc
                                   (Attribute.get Attrs.td_version type_decl)]
                           ; Rpc.Types.constructor=
                               (fun getter ->
                                 let open Rresult.R in
                                 [%e construct_record] ) }
                           : [%t core_type] Rpc.Types.structure )]) ]
      | Ptype_abstract, None -> failwith "Unhandled"
      | Ptype_open, _ -> failwith "Unhandled"
      | Ptype_variant constrs, _ ->
          let default_case = Attribute.get Attrs.td_default type_decl in
          let tcls =
            pexp_construct (Located.mk (lident (String.uppercase name))) None
          in
          let cases =
            List.map
              ~f:(fun constructor ->
                let {pcd_name= {txt= cname; _}; pcd_args; _} = constructor in
                let version =
                  expr_of_option ~loc
                  @@ Attribute.get Attrs.constr_version constructor
                in
                let doc =
                  Common.get_doc ~loc
                    (Attribute.get Attrs.constr_doc constructor)
                    constructor.pcd_attributes
                in
                let rpc_name =
                  Option.value
                    (Attribute.get Attrs.constr_name constructor)
                    ~default:cname
                in
                analyse_variant_case ~loc false tcls
                  (List.length constrs > 1)
                  cname pcd_args version doc rpc_name )
              constrs
          in
          let default =
            [ case ~lhs:ppat_any ~guard:None
                ~rhs:
                  ( match default_case with
                  | None ->
                      [%expr
                        Rresult.R.error_msg
                          (Printf.sprintf "Unknown tag '%s'" s)]
                  | Some d -> [%expr Result.Ok [%e d]] ) ]
          in
          let version = Attribute.get Attrs.td_version type_decl in
          let vconstructor =
            [%expr
              fun s' t ->
                let s = String.lowercase_ascii s' in
                [%e pexp_match (evar "s") (List.map ~f:snd cases @ default)]]
          in
          [ value_binding ~pat:(pvar typ_of_lid)
              ~expr:
                (polymorphize
                   [%expr
                     Rpc.Types.Variant
                       ( { Rpc.Types.vname= [%e estring name]
                         ; Rpc.Types.variants=
                             [%e elist (List.map ~f:fst cases)]
                         ; Rpc.Types.vdefault=
                             [%e expr_of_option ~loc default_case]
                         ; Rpc.Types.vversion= [%e expr_of_option ~loc version]
                         ; Rpc.Types.vconstructor= [%e vconstructor] }
                         : [%t core_type] Rpc.Types.variant )]) ]
    in
    let doc =
      get_doc ~loc
        (Attribute.get Attrs.td_doc type_decl)
        type_decl.ptype_attributes
    in
    let name = type_decl.ptype_name.txt in
    typ_of
    @ [ value_binding ~pat:(pvar name)
          ~expr:
            (polymorphize
               (pexp_record
                  (List.map
                     ~f:(fun (x, y) ->
                       (Located.mk (Ldot (Ldot (Lident "Rpc", "Types"), x)), y)
                       )
                     [ ("name", estring name)
                     ; ("description", doc)
                     ; ( "ty"
                       , poly_apply_of_type_decl ~loc type_decl
                           (pexp_ident (Located.mk (lident typ_of_lid))) ) ])
                  None)) ]
end

let cls_decl ~loc td =
  let module Ast_builder = (val Ast_builder.make loc) in
  let open Ast_builder in
  let core_type = core_type_of_type_declaration td in
  let name = td.ptype_name.txt in
  match (td.ptype_kind, td.ptype_manifest) with
  | Ptype_abstract, Some _ -> []
  | Ptype_record _, _ | Ptype_variant _, _ ->
      let clsval =
        ppat_construct (Located.mk (lident (String.uppercase name))) None
      in
      let matchval =
        [%expr
          match (x, y) with
          | [%p clsval], [%p clsval] -> Some Rpc.Types.Eq
          | _ -> None]
      in
      let typext =
        pstr_typext
          (Ast_helper.Te.mk ~params:[(ptyp_any, Invariant)]
             (Located.mk (Ldot (Ldot (lident "Rpc", "Types"), "cls")))
             [ Ast_helper.Te.decl ~loc
                 ~res:[%type: [%t core_type] Rpc.Types.cls]
                 {txt= String.uppercase name; loc} ])
      in
      let register_eq =
        [%expr
          let eq : type a b.
              a Rpc.Types.cls -> b Rpc.Types.cls -> (a, b) Rpc.Types.eq option
              =
           fun x y -> [%e matchval]
          in
          Rpc.Types.register_eq {Rpc.Types.eq}]
      in
      let register_pr =
        [%expr
          let pr : type a. a Rpc.Types.cls -> string option =
           fun x ->
            match x with
            | [%p clsval] ->
                Some
                  [%e
                    pexp_constant
                      (Pconst_string (String.capitalize name, None))]
            | _ -> None
          in
          Rpc.Types.register_pr {Rpc.Types.pr}]
      in
      [ typext
      ; pstr_value Nonrecursive [value_binding ~pat:ppat_any ~expr:register_eq]
      ; pstr_value Nonrecursive [value_binding ~pat:ppat_any ~expr:register_pr]
      ]
  | _ -> []

let my_str_type_decl ~loc ~path:_ (rec_flag, tds) =
  let cls_decls = List.concat (List.map ~f:(cls_decl ~loc) tds) in
  let typ_and_def_decls =
    pstr_value_list ~loc rec_flag
      (List.concat (List.map ~f:(Typ_of.str_of_type loc) tds))
  in
  cls_decls @ typ_and_def_decls

let str_type_decl = Deriving.Generator.make_noarg my_str_type_decl

let deriver = Deriving.add deriver ~str_type_decl
