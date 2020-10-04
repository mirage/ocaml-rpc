open Ppxlib
open Ast_builder.Default
open Common

let argn = Printf.sprintf "a%d"
let rpc_of str = "rpc_of_" ^ str
let of_rpc str = str ^ "_of_rpc"

let map_lident f = function
  | Lident x -> Lident (f x)
  | Ldot (path, lid) -> Ldot (path, f lid)
  | Lapply _ -> Location.raise_errorf "rpcty - map_lident: Lapply unhandled"


(* [is_option typ] returns true if the type 'typ' is an option type.
   This is required because of the slightly odd way we serialise records containing optional fields. *)
let is_option typ =
  match typ with
  | [%type: [%t? _] option] -> true
  | _ -> false


(* When marshalling (foo * bar) lists we check to see whether it can be better represented by a
   dictionary - we do this by checking (possibly at run time) whether the 'foo' can be unmarshalled from
   a string - this following function, given the type 'foo', returns the run time check *)
let is_string loc typ =
  match typ with
  | [%type: string] -> [%expr true]
  | [%type: int] -> [%expr false]
  | [%type: bool] -> [%expr false]
  | { ptyp_desc = Ptyp_constr (lid, []); _ } ->
    [%expr
      let open Rpc in
      try
        let _ = [%e type_constr_conv lid ~loc ~f:of_rpc [ [%expr Rpc.String ""] ]] in
        true
      with
      | _ -> false]
  | _ -> [%expr false]


let is_dict loc attr =
  match Attribute.get Attrs.is_dict attr with
  | Some () -> [%expr true]
  | None -> [%expr false]


module Of_rpc = struct
  let rec expr_of_typ ~loc typ =
    let module Ast_builder = (val Ast_builder.make loc) in
    let open Ast_builder in
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, _); _ }
      when list_assoc_mem (core_types loc) ~equal:String.equal name ->
      type_constr_conv (Located.mk (Ldot (Lident "Rpc", name))) ~f:of_rpc []
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "char"; _ }, _); _ } ->
      [%expr Rpc.char_of_rpc]
    | { ptyp_desc =
          Ptyp_constr
            ({ txt = Lident "list"; _ }, [ { ptyp_desc = Ptyp_tuple [ typ1; typ2 ]; _ } ])
      ; _
      } ->
      [%expr
        if [%e is_dict loc typ] || [%e is_string loc typ1]
        then
          function
          | Rpc.Dict l ->
            Rpcmarshal.tailrec_map
              (fun (k, v) ->
                [%e expr_of_typ ~loc typ1] (Rpc.String k), [%e expr_of_typ ~loc typ2] v)
              l
          | y ->
            failwith
              (Printf.sprintf "Expecting Rpc.Dict, but found '%s'" (Rpc.to_string y))
        else
          function
          | Rpc.Enum l ->
            Rpcmarshal.tailrec_map
              (function
                | Rpc.Enum [ k; v ] ->
                  [%e expr_of_typ ~loc typ1] k, [%e expr_of_typ ~loc typ2] v
                | y ->
                  failwith
                    (Printf.sprintf
                       "Expecting Rpc.Enum (within an Enum), but found '%s'"
                       (Rpc.to_string y)))
              l
          | y ->
            failwith
              (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]
    (* Tuple lists might be representable by a dictionary, if the first type in the tuple is string-like *)
    | [%type: [%t? typ] list] ->
      [%expr
        function
        | Rpc.Enum l -> Rpcmarshal.tailrec_map [%e expr_of_typ ~loc typ] l
        | y ->
          failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]
    | [%type: [%t? typ] array] ->
      [%expr
        function
        | Rpc.Enum l ->
          Rpcmarshal.tailrec_map [%e expr_of_typ ~loc typ] l |> Array.of_list
        | y ->
          failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]
    | { ptyp_desc = Ptyp_tuple typs; _ } ->
      let pattern = ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) typs in
      let exprs =
        ListLabels.mapi
          ~f:(fun i typ -> [%expr [%e expr_of_typ ~loc typ] [%e evar (argn i)]])
          typs
      in
      [%expr
        function
        | Rpc.Enum [%p plist pattern] -> [%e pexp_tuple exprs]
        | y ->
          failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]
    | [%type: [%t? typ] option] ->
      let e = expr_of_typ ~loc typ in
      [%expr
        function
        | Rpc.Enum [] -> None
        | Rpc.Enum [ y ] -> Some ([%e e] y)
        | y ->
          failwith (Printf.sprintf "Expecting Rpc.Enum, but found '%s'" (Rpc.to_string y))]
    | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
      let args =
        List.rev @@ ListLabels.rev_map ~f:(fun x -> Nolabel, expr_of_typ ~loc x) args
      in
      let f = pexp_ident (Located.mk (map_lident of_rpc lid)) in
      pexp_apply f args
    | { ptyp_desc = Ptyp_var name; _ } -> [%expr [%e evar ("poly_" ^ name)]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); _ } ->
      let inherits, tags =
        list_partition_tf
          ~f:(function
            | { prf_desc = Rinherit _; _ } -> true
            | _ -> false)
          fields
      in
      let bad = [%expr failwith "Unknown tag/contents"] in
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
        |> ListLabels.rev_map ~f:(fun field ->
               let { prf_desc; _ } = field in
               match prf_desc with
               | Rtag (label, true, []) ->
                 let label' = String.lowercase_ascii label.txt in
                 let name =
                   match Attribute.get Attrs.rt_name field with
                   | Some s -> s
                   | None -> label'
                 in
                 case
                   ~lhs:[%pat? Rpc.String [%p pstring name]]
                   ~guard:None
                   ~rhs:(pexp_variant label.txt None)
               | Rtag (label, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
                 let label' = String.lowercase_ascii label.txt in
                 let name =
                   match Attribute.get Attrs.rt_name field with
                   | Some s -> s
                   | None -> label'
                 in
                 let exprs =
                   ListLabels.mapi
                     ~f:(fun i typ ->
                       [%expr [%e expr_of_typ ~loc typ] [%e evar (argn i)]])
                     typs
                 in
                 case
                   ~lhs:
                     [%pat?
                       Rpc.Enum
                         [ Rpc.String [%p pstring name]
                         ; Rpc.Enum
                             [%p
                               plist (ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) typs)]
                         ]]
                   ~guard:None
                   ~rhs:(pexp_variant label.txt (Some (pexp_tuple exprs)))
               | Rtag (label, false, [ typ ]) ->
                 let label' = String.lowercase_ascii label.txt in
                 let name =
                   match Attribute.get Attrs.rt_name field with
                   | Some s -> s
                   | None -> label'
                 in
                 case
                   ~lhs:[%pat? Rpc.Enum [ Rpc.String [%p pstring name]; y ]]
                   ~guard:None
                   ~rhs:
                     [%expr
                       [%e expr_of_typ ~loc typ] y
                       |> fun x -> [%e pexp_variant label.txt (Some [%expr x])]]
               | _ -> failwith "Cannot derive variant case")
        |> List.rev
      and inherits_case =
        (*let toplevel_typ = typ in*)
        let expr =
          List.rev
          @@ ListLabels.rev_map
               ~f:(function
                 | { prf_desc = Rinherit typ; _ } -> typ
                 | _ -> assert false)
               inherits
          |> ListLabels.fold_left
               ~f:(fun expr typ ->
                 [%expr
                   try [%e expr_of_typ ~loc typ] rpc (*  :> [%t toplevel_typ]*) with
                   | _ -> [%e expr]])
               ~init:default_expr
        in
        case ~lhs:[%pat? _] ~guard:None ~rhs:expr
      in
      [%expr
        fun (rpc : Rpc.t) ->
          let rpc' =
            match rpc with
            | Rpc.Enum (Rpc.String x :: xs) ->
              Rpc.Enum (Rpc.String (String.lowercase_ascii x) :: xs)
            | Rpc.String x -> Rpc.String (String.lowercase_ascii x)
            | y -> y
          in
          [%e pexp_match [%expr rpc'] (tag_cases @ [ inherits_case ])]]
    | { ptyp_desc = Ptyp_any; _ } -> failwith "Ptyp_any not handled"
    | { ptyp_desc = Ptyp_poly (_, _); _ } -> failwith "Ptyp_poly not handled"
    | { ptyp_desc = Ptyp_extension _; _ } -> failwith "Ptyp_extension not handled"
    | { ptyp_desc = Ptyp_arrow (_, _, _); _ } -> failwith "Ptyp_arrow not handled"
    | { ptyp_desc = Ptyp_object (_, _); _ } -> failwith "Ptyp_object not handled"
    | { ptyp_desc = Ptyp_alias (_, _); _ } -> failwith "Ptyp_alias not handled"
    | { ptyp_desc = Ptyp_class (_, _); _ } -> failwith "Ptyp_class not handled"
    | { ptyp_desc = Ptyp_package _; _ } -> failwith "Ptyp_package not handled"


  let str_of_type ~loc type_decl =
    let module Ast_builder = (val Ast_builder.make loc) in
    let open Ast_builder in
    let of_rpc =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest -> expr_of_typ ~loc manifest
      | Ptype_record labels, _ ->
        let record =
          ListLabels.fold_left
            ~f:(fun expr (i, label) ->
              let { pld_name = { txt = name; _ }; _ } = label in
              let key =
                String.lowercase_ascii
                  (match Attribute.get Attrs.key label with
                  | Some s -> s
                  | None -> name)
              in
              let pat = pvar (argn i) in
              let expr' = evar (argn i) in
              let str = estring key in
              [%expr
                let [%p pat] =
                  match [%e expr'] with
                  | Some x -> x
                  | None ->
                    failwith (Printf.sprintf "Undefined field: Expecting '%s'" [%e str])
                in
                [%e expr]])
            ~init:
              [%expr
                [%e
                  pexp_record
                    (labels
                    |> ListLabels.mapi ~f:(fun i { pld_name = { txt = name; _ }; _ } ->
                           { txt = Lident name; loc }, evar (argn i)))
                    None]]
            (labels |> ListLabels.mapi ~f:(fun i label -> i, label))
        in
        let wrap_opt pld_type x =
          if is_option pld_type then [%expr Rpc.Enum [ [%e x] ]] else x
        in
        let cases =
          (labels
          |> ListLabels.mapi ~f:(fun i label ->
                 let { pld_name = { txt = name; _ }; pld_type; _ } = label in
                 let key =
                   String.lowercase_ascii
                     (match Attribute.get Attrs.key label with
                     | Some s -> s
                     | None -> name)
                 in
                 let thunks =
                   labels
                   |> ListLabels.mapi ~f:(fun j _ ->
                          if i = j
                          then
                            [%expr
                              Some
                                [%e
                                  pexp_apply
                                    (expr_of_typ ~loc pld_type)
                                    [ Nolabel, wrap_opt pld_type (evar "x") ]]]
                          else evar (argn j))
                 in
                 case
                   ~lhs:[%pat? ([%p pstring key], x) :: xs]
                   ~guard:None
                   ~rhs:[%expr loop xs [%e pexp_tuple thunks]]))
          @ [ case ~lhs:[%pat? []] ~guard:None ~rhs:record
            ; case ~lhs:[%pat? _ :: xs] ~guard:None ~rhs:[%expr loop xs _state]
            ]
        and thunks =
          labels
          |> ListLabels.rev_map ~f:(fun { pld_name = _; pld_type; _ } ->
                 if is_option pld_type then [%expr Some None] else [%expr None])
          |> List.rev
        in
        [%expr
          fun x ->
            match x with
            | Rpc.Dict dict ->
              let d' =
                List.rev
                @@ ListLabels.rev_map ~f:(fun (k, v) -> String.lowercase_ascii k, v) dict
              in
              let rec loop
                  xs
                  ([%p ppat_tuple (ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) labels)]
                  as _state)
                =
                [%e pexp_match [%expr xs] cases]
              in
              loop d' [%e pexp_tuple thunks]
            | y ->
              failwith
                (Printf.sprintf "Expecting Rpc.Dict, but found '%s'" (Rpc.to_string y))]
      | Ptype_abstract, None -> failwith "Unhandled"
      | Ptype_open, _ -> failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs
          |> ListLabels.rev_map ~f:(fun constr ->
                 let { pcd_name = { txt = name; _ }; pcd_args; _ } = constr in
                 let name' =
                   match Attribute.get Attrs.constr_name constr with
                   | Some n -> n
                   | None -> name
                 in
                 match pcd_args with
                 | Pcstr_tuple typs ->
                   let subpattern =
                     ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) typs |> plist
                   in
                   let exprs =
                     ListLabels.mapi
                       ~f:(fun i typ ->
                         [%expr [%e expr_of_typ ~loc typ] [%e evar (argn i)]])
                       typs
                   in
                   let contents =
                     match exprs with
                     | [] -> None
                     | [ x ] -> Some x
                     | xs -> Some (pexp_tuple xs)
                   in
                   let rpc_of = pexp_construct { txt = Lident name; loc } contents in
                   let main =
                     [%pat? Rpc.String [%p pstring (String.lowercase_ascii name')]]
                   in
                   let pattern =
                     match typs with
                     | [] -> main
                     | _ -> [%pat? Rpc.Enum ([%p main] :: [%p subpattern])]
                   in
                   case ~lhs:pattern ~guard:None ~rhs:rpc_of
                 | Pcstr_record _ -> failwith "record variants are not supported")
          |> List.rev
        in
        let default =
          case
            ~lhs:[%pat? y]
            ~guard:None
            ~rhs:
              [%expr
                failwith
                  (Printf.sprintf
                     "Unhandled pattern when unmarshalling variant type: found '%s'"
                     (Rpc.to_string y))]
        in
        [%expr
          fun rpc ->
            let rpc' = Rpc.lowerfn rpc in
            [%e pexp_function (cases @ [ default ])] rpc']
    in
    of_rpc
end

module Rpc_of = struct
  let rec expr_of_typ ~loc typ =
    let module Ast_builder = (val Ast_builder.make loc) in
    let open Ast_builder in
    match typ with
    | { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, _); _ }
      when list_assoc_mem (Common.core_types loc) ~equal:String.equal name ->
      [%expr Rpc.([%e pexp_ident (Located.mk (lident (rpc_of name)))])]
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "char"; _ }, _); _ } ->
      [%expr
        Rpc.(
          function
          | c -> Rpc.Int (Int64.of_int (Char.code c)))]
    (* Tuple lists might be representable by a dictionary, if the first type in the tuple is string-like *)
    | { ptyp_desc =
          Ptyp_constr
            ({ txt = Lident "list"; _ }, [ { ptyp_desc = Ptyp_tuple [ typ1; typ2 ]; _ } ])
      ; _
      } ->
      [%expr
        if [%e is_dict loc typ] || [%e is_string loc typ1]
        then
          fun l ->
          Rpc.Dict
            (List.rev
            @@ ListLabels.rev_map
                 ~f:(fun (k, v) ->
                   ( Rpc.string_of_rpc ([%e expr_of_typ ~loc typ1] k)
                   , [%e expr_of_typ ~loc typ2] v ))
                 l)
        else
          fun l ->
          Rpc.Enum
            (List.rev
            @@ ListLabels.rev_map
                 ~f:(fun (a, b) ->
                   Rpc.Enum [ [%e expr_of_typ ~loc typ1] a; [%e expr_of_typ ~loc typ2] b ])
                 l)]
    | [%type: [%t? typ] list] ->
      [%expr fun l -> Rpc.Enum (Rpcmarshal.tailrec_map [%e expr_of_typ ~loc typ] l)]
    | [%type: [%t? typ] array] ->
      [%expr
        fun l ->
          Rpc.Enum (Rpcmarshal.tailrec_map [%e expr_of_typ ~loc typ] (Array.to_list l))]
    | { ptyp_desc = Ptyp_tuple typs; _ } ->
      let args =
        ListLabels.mapi
          ~f:(fun i typ -> pexp_apply (expr_of_typ ~loc typ) [ Nolabel, evar (argn i) ])
          typs
      in
      [%expr
        fun [%p ppat_tuple (ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) typs)] ->
          Rpc.Enum [%e elist args]]
    | [%type: [%t? typ] option] ->
      let e = expr_of_typ ~loc typ in
      [%expr
        fun x ->
          match x with
          | None -> Rpc.Enum []
          | Some y -> Rpc.Enum [ [%e e] y ]]
    | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
      let args =
        List.rev @@ ListLabels.rev_map ~f:(fun e -> Nolabel, expr_of_typ ~loc e) args
      in
      let f = pexp_ident (Located.mk (map_lident rpc_of lid)) in
      pexp_apply f args
    | { ptyp_desc = Ptyp_variant (fields, _, _); _ } ->
      let cases =
        fields
        |> ListLabels.rev_map ~f:(fun field ->
               let { prf_desc; _ } = field in
               match prf_desc with
               | Rtag (label, true, []) ->
                 let l =
                   match Attribute.get Attrs.rt_name field with
                   | Some x -> x
                   | None -> label.txt
                 in
                 case
                   ~lhs:(ppat_variant label.txt None)
                   ~guard:None
                   ~rhs:[%expr Rpc.String [%e estring l]]
               | Rtag (label, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
                 let l =
                   elist
                     (ListLabels.mapi
                        ~f:(fun i typ ->
                          pexp_apply (expr_of_typ ~loc typ) [ Nolabel, evar (argn i) ])
                        typs)
                 in
                 let label =
                   match Attribute.get Attrs.rt_name field with
                   | Some x -> x
                   | None -> label.txt
                 in
                 case
                   ~lhs:
                     (ppat_variant
                        label
                        (ppat_tuple_opt
                           (ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) typs)))
                   ~guard:None
                   ~rhs:
                     [%expr Rpc.Enum [ Rpc.String [%e estring label]; Rpc.Enum [%e l] ]]
               | Rtag (label, false, [ typ ]) ->
                 let label =
                   match Attribute.get Attrs.rt_name field with
                   | Some x -> x
                   | None -> label.txt
                 in
                 case
                   ~lhs:(ppat_variant label (Some [%pat? x]))
                   ~guard:None
                   ~rhs:
                     [%expr
                       Rpc.Enum
                         [ Rpc.String [%e estring label]; [%e expr_of_typ ~loc typ] x ]]
               | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _); _ } as typ) ->
                 case
                   ~lhs:[%pat? [%p ppat_type tname] as x]
                   ~guard:None
                   ~rhs:[%expr [%e expr_of_typ ~loc typ] x]
               | _ -> failwith "cannot be derived for")
        |> List.rev
      in
      pexp_function cases
    | { ptyp_desc = Ptyp_any; _ } -> failwith "Ptyp_any not handled"
    | { ptyp_desc = Ptyp_var name; _ } -> [%expr [%e evar ("poly_" ^ name)]]
    | { ptyp_desc = Ptyp_poly (_, _); _ } -> failwith "Ptyp_poly not handled"
    | { ptyp_desc = Ptyp_extension _; _ } -> failwith "Ptyp_extension not handled"
    | { ptyp_desc = Ptyp_arrow (_, _, _); _ } -> failwith "Ptyp_arrow not handled"
    | { ptyp_desc = Ptyp_object (_, _); _ } -> failwith "Ptyp_object not handled"
    | { ptyp_desc = Ptyp_alias (_, _); _ } -> failwith "Ptyp_alias not handled"
    | { ptyp_desc = Ptyp_class (_, _); _ } -> failwith "Ptyp_class not handled"
    | { ptyp_desc = Ptyp_package _; _ } -> failwith "Ptyp_package not handled"


  (*  | _ -> failwith "Error"*)

  let str_of_type ~loc type_decl =
    let module Ast_builder = (val Ast_builder.make loc) in
    let open Ast_builder in
    let to_rpc =
      match type_decl.ptype_kind, type_decl.ptype_manifest with
      | Ptype_abstract, Some manifest -> expr_of_typ ~loc manifest
      | Ptype_record labels, _ ->
        let fields =
          labels
          |> ListLabels.rev_map ~f:(fun label ->
                 let { pld_name = { txt = name; _ }; pld_type; _ } = label in
                 let rpc_name =
                   match Attribute.get Attrs.key label with
                   | Some s -> s
                   | None -> name
                 in
                 if is_option pld_type
                 then
                   [%expr
                     let rpc =
                       [%e expr_of_typ ~loc pld_type]
                         [%e pexp_field (evar "x") { txt = Lident name; loc }]
                     in
                     match rpc with
                     | Rpc.Enum [ x ] -> Some ([%e estring rpc_name], x)
                     | Rpc.Enum [] -> None
                     | _ ->
                       failwith
                         (Printf.sprintf
                            "Programmer error when marshalling %s.%s"
                            [%e estring type_decl.ptype_name.txt]
                            [%e estring name])]
                   (* Should never happen *)
                 else
                   [%expr
                     Some
                       ( [%e estring rpc_name]
                       , [%e expr_of_typ ~loc pld_type]
                           [%e pexp_field (evar "x") { txt = Lident name; loc }] )])
          |> List.rev
        in
        [%expr
          fun x ->
            Rpc.Dict
              (ListLabels.fold_right
                 ~f:(fun x acc ->
                   match x with
                   | Some x -> x :: acc
                   | None -> acc)
                 [%e elist fields]
                 ~init:[])]
      | Ptype_abstract, None -> failwith "Unhandled"
      | Ptype_open, _ -> failwith "Unhandled"
      | Ptype_variant constrs, _ ->
        let cases =
          constrs
          |> ListLabels.rev_map ~f:(fun constr ->
                 let { pcd_name = { txt = name; _ }; pcd_args; _ } = constr in
                 match pcd_args with
                 | Pcstr_tuple typs ->
                   let args =
                     ListLabels.mapi
                       ~f:(fun i typ ->
                         [%expr [%e expr_of_typ ~loc typ] [%e evar (argn i)]])
                       typs
                   in
                   let argsl = elist args in
                   let pattern = ListLabels.mapi ~f:(fun i _ -> pvar (argn i)) typs in
                   let name' =
                     match Attribute.get Attrs.constr_name constr with
                     | Some s -> s
                     | None -> name
                   in
                   let rpc_of =
                     match args with
                     | [] -> [%expr Rpc.String [%e estring name']]
                     | _ -> [%expr Rpc.Enum (Rpc.String [%e estring name'] :: [%e argsl])]
                   in
                   case
                     ~lhs:
                       (ppat_construct
                          { txt = Lident name; loc }
                          (ppat_tuple_opt pattern))
                     ~guard:None
                     ~rhs:rpc_of
                 | Pcstr_record _ -> failwith "record variants are not supported")
          |> List.rev
        in
        pexp_function cases
    in
    to_rpc
end

let rpc_strs_of_type ~loc type_decl =
  let polymorphize = Common.poly_fun_of_type_decl type_decl in
  let name = type_decl.ptype_name.txt in
  [ value_binding
      ~loc
      ~pat:(pvar ~loc (rpc_of name))
      ~expr:
        (pexp_fun
           ~loc
           Nolabel
           None
           (pvar ~loc "__x__")
           [%expr [%e polymorphize ~loc (Rpc_of.str_of_type ~loc type_decl)] __x__])
  ; value_binding
      ~loc
      ~pat:(pvar ~loc (of_rpc name))
      ~expr:
        (pexp_fun
           ~loc
           Nolabel
           None
           (pvar ~loc "__x__")
           [%expr [%e polymorphize ~loc (Of_rpc.str_of_type ~loc type_decl)] __x__])
  ]


let my_str_type_decl ~loc ~path:_ (rec_flag, tds) =
  pstr_value_list
    ~loc
    rec_flag
    (List.concat (List.rev @@ ListLabels.rev_map ~f:(rpc_strs_of_type ~loc) tds))


let str_type_decl = Deriving.Generator.make_noarg my_str_type_decl
let deriver = Deriving.add "rpc" ~str_type_decl
