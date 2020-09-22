open Ppxlib
open Ast_builder.Default

let list_assoc_find_exn t ~equal key =
  match List.find_opt (fun (key', _) -> equal key key') t with
  | None -> raise Not_found
  | Some x -> snd x


let list_assoc_mem t ~equal key =
  match List.find_opt (fun (key', _) -> equal key key') t with
  | None -> false
  | Some _ -> true


let string_concat ?(sep = "") l =
  match l with
  | [] -> ""
  (* The stdlib does not specialize this case because it could break existing projects. *)
  | [ x ] -> x
  | l -> String.concat sep l


let string_split_on_chars str ~on =
  let rec char_list_mem l (c : char) =
    match l with
    | [] -> false
    | hd :: tl -> Char.equal hd c || char_list_mem tl c
  in
  let split_gen str ~on =
    let is_delim c = char_list_mem on c in
    let len = String.length str in
    let rec loop acc last_pos pos =
      if pos = -1
      then String.sub str 0 last_pos :: acc
      else if is_delim str.[pos]
      then (
        let pos1 = pos + 1 in
        let sub_str = String.sub str pos1 (last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1))
      else loop acc last_pos (pos - 1)
    in
    loop [] len (len - 1)
  in
  split_gen str ~on


let is_whitespace = function
  | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' -> true
  | _ -> false


let string_strip ?(drop = is_whitespace) t =
  let lfindi ?(pos = 0) t ~f =
    let n = String.length t in
    let rec loop i = if i = n then None else if f i t.[i] then Some i else loop (i + 1) in
    loop pos
  in
  let rfindi ?pos t ~f =
    let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
    let pos =
      match pos with
      | Some pos -> pos
      | None -> String.length t - 1
    in
    loop pos
  in
  let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c)) in
  let first_non_drop ~drop t = lfindi t ~f:(fun _ c -> not (drop c)) in
  let length = String.length t in
  if length = 0 || not (drop t.[0] || drop t.[length - 1])
  then t
  else (
    match first_non_drop t ~drop with
    | None -> ""
    | Some first ->
      (match last_non_drop t ~drop with
      | None -> assert false
      | Some last -> String.sub t first (last - first + 1)))


let list_partition_tf t ~f =
  let partition_map t ~f =
    let rec loop t fst snd =
      match t with
      | [] -> List.rev fst, List.rev snd
      | x :: t ->
        (match f x with
        | Ok y -> loop t (y :: fst) snd
        | Error y -> loop t fst (y :: snd))
    in
    loop t [] []
  in
  let f x = if f x then Ok x else Error x in
  partition_map t ~f


let core_types loc =
  ListLabels.map
    ~f:(fun (s, y) -> s, y)
    [ "unit", [%expr Rpc.Types.Unit]
    ; "int", [%expr Rpc.Types.(Basic Int)]
    ; "int32", [%expr Rpc.Types.(Basic Int32)]
    ; "int64", [%expr Rpc.Types.(Basic Int64)]
    ; "string", [%expr Rpc.Types.(Basic String)]
    ; "float", [%expr Rpc.Types.(Basic Float)]
    ; "bool", [%expr Rpc.Types.(Basic Bool)]
    ]


(** Many of the following functions are lifted from ppx_deriving. It's quite likely that
    there are good alternatives to these somewhere in ppxlib, but I've not yet found them.

    They are used to deal with parameterised types. When declaring a function derived from
    a parameterised type, the function will be extended to take an argument for each
    type parameter. The important functions below are `poly_fun_of_type_decl` and
    `poly_apply_of_type_decl` - for declaring and using the derived functions respectively.
*)

let fold_right_type_params fn params accum =
  ListLabels.fold_right
    ~f:(fun (param, _) accum ->
      match param with
      | { ptyp_desc = Ptyp_any; _ } -> accum
      | { ptyp_desc = Ptyp_var name; _ } -> fn name accum
      | _ -> assert false)
    params
    ~init:accum


(** [fold_right_type_decl fn accum type_] performs a right fold over all type variable
    (i.e. not wildcard) parameters in [type_]. *)
let fold_right_type_decl fn { ptype_params; _ } accum =
  fold_right_type_params fn ptype_params accum


(** [poly_fun_of_type_decl type_ expr] wraps [expr] into [fun poly_N -> ...] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [fun poly_a poly_b -> [%e expr]]. *)
let poly_fun_of_type_decl ~loc type_decl expr =
  fold_right_type_decl
    (fun name expr ->
      pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = "poly_" ^ name; loc }) expr)
    type_decl
    expr


let fold_left_type_params fn accum params =
  ListLabels.fold_left
    ~f:(fun accum (param, _) ->
      match param with
      | { ptyp_desc = Ptyp_any; _ } -> accum
      | { ptyp_desc = Ptyp_var name; _ } -> fn accum name
      | _ -> assert false)
    ~init:accum
    params


(** [fold_left_type_decl fn accum type_] performs a left fold over all type variable
    (i.e. not wildcard) parameters in [type_]. *)
let fold_left_type_decl fn accum { ptype_params; _ } =
  fold_left_type_params fn accum ptype_params


(** [poly_apply_of_type_decl type_ expr] wraps [expr] into [expr poly_N] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [[%e expr] poly_a poly_b].
    [_] parameters are ignored. *)
let poly_apply_of_type_decl ~loc type_decl expr =
  fold_left_type_decl
    (fun expr name -> Ast_helper.Exp.apply expr [ Nolabel, evar ~loc ("poly_" ^ name) ])
    expr
    type_decl


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
    Attribute.declare
      "rpc.default"
      context
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)


  let label_default = default Attribute.Context.label_declaration
  let td_default = default Attribute.Context.type_declaration
  let ct_default = default Attribute.Context.core_type
  let rtag_default = default Attribute.Context.rtag

  let doc context =
    Attribute.declare
      "rpc.doc"
      context
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)


  let label_doc = doc Attribute.Context.label_declaration
  let constr_doc = doc Attribute.Context.constructor_declaration
  let td_doc = doc Attribute.Context.type_declaration

  let version context =
    Attribute.declare
      "rpc.version"
      context
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)


  let label_version = version Attribute.Context.label_declaration
  let td_version = version Attribute.Context.type_declaration
  let constr_version = version Attribute.Context.constructor_declaration

  let label_typ =
    Attribute.declare
      "rpc.typ"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
      (fun x -> x)


  let name context =
    Attribute.declare
      "rpc.name"
      context
      Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ __ none)) nil ^:: nil))
      (fun x _loc -> x)


  let constr_name = name Attribute.Context.constructor_declaration
  let rt_name = name Attribute.Context.rtag

  let key =
    Attribute.declare
      "rpc.key"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ __ none)) nil ^:: nil))
      (fun x _loc -> x)


  let is_dict =
    Attribute.declare "rpc.dict" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
end

(* The following functions are for extracting `ocaml.doc` attributes from the AST. These are
   captured and used for 'doc' fields of the generated values representing the types. Ppxlib
   seems to object to using these attributes as they are 'already in use', so we don't get to
   use the nice `Attributes` module and have to roll our own. *)
let attr loc name attrs =
  let pat =
    Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ __ none)) __ ^:: nil))
  in
  List.find_opt (fun { attr_name = { txt; _ }; _ } -> String.equal txt name) attrs
  |> Option.map (fun { attr_payload; _ } -> attr_payload)
  |> fun o ->
  Option.bind o (fun str ->
      Ast_pattern.parse pat loc str ~on_error:(fun _ -> None) (fun str _loc _ -> Some str))


let split = string_split_on_chars ~on:[ '\n' ]

let convert_doc x =
  split x
  |> ListLabels.map
       ~f:
         (string_strip ~drop:(function
             | '\n' | ' ' -> true
             | _ -> false))


(** [get_doc loc rpcdoc attrs] extracts documentation from the type declarations. rpcdoc is
    the result of looking for \@doc tags. If this is found, we use that. If not, we look for
    ocamldoc docstrings and return them instead. In both cases, the result is an expression of
    type list *)
let get_doc ~loc rpcdoc (attrs : attributes) =
  let ocamldoc = attr loc "ocaml.doc" attrs in
  match rpcdoc, ocamldoc with
  | Some e, _ -> e
  | _, Some s -> elist ~loc (convert_doc s |> ListLabels.map ~f:(estring ~loc))
  | _, _ -> elist ~loc []
