open Base
open Ppxlib
open Ast_builder.Default

let core_types loc =
  List.map
    ~f:(fun (s, y) -> (s, y))
    [ ("unit", [%expr Rpc.Types.Unit])
    ; ("int", [%expr Rpc.Types.(Basic Int)])
    ; ("int32", [%expr Rpc.Types.(Basic Int32)])
    ; ("int64", [%expr Rpc.Types.(Basic Int64)])
    ; ("string", [%expr Rpc.Types.(Basic String)])
    ; ("float", [%expr Rpc.Types.(Basic Float)])
    ; ("bool", [%expr Rpc.Types.(Basic Bool)]) ]

(** Many of the following functions are lifted from ppx_deriving. It's quite likely that
    there are good alternatives to these somewhere in ppxlib, but I've not yet found them.

    They are used to deal with parameterised types. When declaring a function derived from
    a parameterised type, the function will be extended to take an argument for each
    type parameter. The important functions below are `poly_fun_of_type_decl` and
    `poly_apply_of_type_decl` - for declaring and using the derived functions respectively.
*)

let fold_right_type_params fn params accum =
  List.fold_right ~f:(fun (param, _) accum ->
      match param with
      | { ptyp_desc = Ptyp_any; _ } -> accum
      | { ptyp_desc = Ptyp_var name; _ } ->
        fn name accum
      | _ -> assert false)
    params ~init:accum

(** [fold_right_type_decl fn accum type_] performs a right fold over all type variable
    (i.e. not wildcard) parameters in [type_]. *)
let fold_right_type_decl fn { ptype_params; _ } accum =
  fold_right_type_params fn ptype_params accum

(** [poly_fun_of_type_decl type_ expr] wraps [expr] into [fun poly_N -> ...] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [fun poly_a poly_b -> [%e expr]]. *)
let poly_fun_of_type_decl ~loc type_decl expr =
  fold_right_type_decl (fun name expr ->
      pexp_fun ~loc Nolabel None (ppat_var ~loc {txt="poly_"^name; loc}) expr) type_decl expr


let fold_left_type_params fn accum params =
  List.fold_left ~f:(fun accum (param, _) ->
      match param with
      | { ptyp_desc = Ptyp_any; _ } -> accum
      | { ptyp_desc = Ptyp_var name; _ } ->
        fn accum name
      | _ -> assert false)
    ~init:accum params

(** [fold_left_type_decl fn accum type_] performs a left fold over all type variable
    (i.e. not wildcard) parameters in [type_]. *)
let fold_left_type_decl fn accum { ptype_params; _ } =
  fold_left_type_params fn accum ptype_params

(** [poly_apply_of_type_decl type_ expr] wraps [expr] into [expr poly_N] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [[%e expr] poly_a poly_b].
    [_] parameters are ignored. *)
let poly_apply_of_type_decl ~loc type_decl expr =
  fold_left_type_decl (fun expr name ->
      Ast_helper.Exp.apply expr [Nolabel, evar ~loc ("poly_"^name)]) expr type_decl


(** [expr_of_option ~loc o] turns an optional expression into an expression
    of an optional value. In several places there are optional attributes,
    e.g. [@@version foo], which end up as values of type `expression option`.
    These are often turned into optional values in the generated code. *)
let expr_of_option ~loc o =
  match o with
  | None -> [%expr None]
  | Some d -> [%expr Some [%e d]]


(** Typed attribute getters *)
module Attrs = struct
  let default context =
    Attribute.declare "rpc.default"
      context
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)

  let label_default = default Attribute.Context.label_declaration
  let td_default = default Attribute.Context.type_declaration
  let ct_default = default Attribute.Context.core_type
  let rtag_default = default Attribute.Context.rtag

  let doc context =
    Attribute.declare "rpc.doc"
      context
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)

  let label_doc = doc Attribute.Context.label_declaration
  let constr_doc = doc Attribute.Context.constructor_declaration
  let td_doc = doc Attribute.Context.type_declaration

  let version context = 
    Attribute.declare "rpc.version"
      context
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)

  let label_version = version Attribute.Context.label_declaration
  let td_version = version Attribute.Context.type_declaration
  let constr_version = version Attribute.Context.constructor_declaration

  let label_typ =
    Attribute.declare "rpc.typ"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)

  let name context =
    Attribute.declare "rpc.name" context
      Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ none)) nil ^:: nil))
      (fun x -> x)

  let constr_name = name       Attribute.Context.constructor_declaration

  let rt_name = name Attribute.Context.rtag


  let key =
    Attribute.declare "rpc.key"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ none)) nil ^:: nil))
      (fun x -> x)

  let is_dict =
    Attribute.declare "rpc.dict"
      Attribute.Context.core_type
      Ast_pattern.(pstr nil)
      ()

end

(* The following functions are for extracting `ocaml.doc` attributes from the AST. These are
   captured and used for 'doc' fields of the generated values representing the types. Ppxlib
   seems to object to using these attributes as they are 'already in use', so we don't get to
   use the nice `Attributes` module and have to roll our own. *)
let attr loc name attrs =
  let pat = Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ none)) __ ^:: nil)) in
  List.find ~f:(fun ({ txt; _ }, _) -> String.equal txt name) attrs
  |> Option.map ~f:(snd)
  |> Option.bind ~f:(fun str -> Ast_pattern.parse pat loc str ~on_error:(fun _ -> None) (fun str _ -> Some str))

let split = String.split_on_chars ~on:['\n']

let convert_doc x = split x |> List.map ~f:(String.strip ~drop:(function | '\n' | ' ' -> true | _ -> false))

(** [get_doc loc rpcdoc attrs] extracts documentation from the type declarations. rpcdoc is
    the result of looking for \@\@doc tags. If this is found, we use that. If not, we look for
    ocamldoc docstrings and return them instead. In both cases, the result is an expression of
    type list *)
let get_doc ~loc rpcdoc (attrs : attributes) =
  let ocamldoc = attr loc "ocaml.doc" attrs in
  match rpcdoc, ocamldoc with
  | Some e, _ -> e
  | _, Some s -> elist ~loc (convert_doc s |> List.map ~f:(estring ~loc))
  | _, _ -> elist ~loc []
