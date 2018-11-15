(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2006-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

let debug = ref false

let set_debug x = debug := x

let get_debug () = !debug

type t =
  | Int of int64
  | Int32 of int32
  | Bool of bool
  | Float of float
  | String of string
  | DateTime of string
  | Enum of t list
  | Dict of (string * t) list
  | Null

module Version = struct
  type t = int * int * int

  let compare (x, y, z) (x', y', z') =
    let cmp a b fn () =
      let c = compare a b in
      if c <> 0 then c else fn ()
    in
    cmp x x' (cmp y y' (cmp z z' (fun () -> 0))) ()
end

module Types = struct
  type _ cls = ..

  type 'a cls += Unnamed : 'a cls

  type (_, _) eq = Eq : ('a, 'a) eq

  type eqcls = {eq: 'a 'b. 'a cls -> 'b cls -> ('a, 'b) eq option}

  type prcls = {pr: 'a. 'a cls -> string option}

  let eqcls =
    let eq : type a b. a cls -> b cls -> (a, b) eq option =
     fun _x _y -> None
    in
    ref {eq}

  let prcls =
    let pr : type a. a cls -> string option = fun _x -> None in
    ref {pr}

  let register_eq {eq} =
    let oldeq = !eqcls in
    let eq : type a b. a cls -> b cls -> (a, b) eq option =
     fun x y -> match eq x y with Some Eq -> Some Eq | None -> oldeq.eq x y
    in
    eqcls := {eq}

  let register_pr {pr} =
    let oldpr = !prcls in
    let pr : type a. a cls -> string option =
     fun x -> match pr x with Some s -> Some s | None -> oldpr.pr x
    in
    prcls := {pr}

  type _ basic =
    | Int : int basic
    | Int32 : int32 basic
    | Int64 : int64 basic
    | Bool : bool basic
    | Float : float basic
    | String : string basic
    | Char : char basic

  type _ typ =
    | Basic : 'a basic -> 'a typ
    | DateTime : string typ
    | Array : 'a typ -> 'a array typ
    | List : 'a typ -> 'a list typ
    | Refv : 'a cls * 'a typ -> 'a ref typ
    | Dict : 'a basic * 'b typ -> ('a * 'b) list typ
    | Unit : unit typ
    | Option : 'a typ -> 'a option typ
    | Tuple : 'a typ * 'b typ -> ('a * 'b) typ
    | Tuple3 : 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) typ
    | Tuple4 : 'a typ * 'b typ * 'c typ * 'd typ -> ('a * 'b * 'c * 'd) typ
    | Struct : 'a structure -> 'a typ
    | Variant : 'a variant -> 'a typ
    | Abstract : 'a abstract -> 'a typ
    | Refmap : 'a typ -> 'a Refmap.t typ

  (* A type definition has a name and description *)
  and 'a def = {name: string; description: string list; ty: 'a typ}

  and boxed_def = BoxedDef : 'a def -> boxed_def

  and ('a, 's) field =
    { fname: string list
    ; fdescription: string list
    ; fversion: Version.t option
    ; field: 'a typ
    ; fdefault: 'a option
    ; fcls: 's cls
    ; fget: 's -> 'a (* Lenses *)
    ; fset: 'a -> 's -> 's }

  and 'a boxed_field = BoxedField : ('a, 's) field -> 's boxed_field

  and field_getter =
    {field_get: 'a. string -> 'a typ -> ('a, Rresult.R.msg) Result.result}

  and 'a structure =
    { sname: string
    ; fields: 'a boxed_field list
    ; version: Version.t option
    ; constructor: field_getter -> ('a, Rresult.R.msg) Result.result }

  and ('a, 's) tag =
    { tname: string
    ; tdescription: string list
    ; tversion: Version.t option
    ; tcontents: 'a typ
    ; tcls: 's cls
    ; tpreview: 's -> 'a option
    ; treview: 'a -> 's }

  and 'a boxed_tag = BoxedTag : ('a, 's) tag -> 's boxed_tag

  and tag_getter = {tget: 'a. 'a typ -> ('a, Rresult.R.msg) Result.result}

  and 'a variant =
    { vname: string
    ; variants: 'a boxed_tag list
    ; vdefault: 'a option
    ; vversion: Version.t option
    ; vconstructor: string -> tag_getter -> ('a, Rresult.R.msg) Result.result
    }

  and 'a abstract =
    { aname: string
    ; acls: 'a cls
    ; test_data: 'a list
    ; rpc_of: 'a -> t
    ; of_rpc: t -> ('a, Rresult.R.msg) Result.result }

  and 'a refs = 'a Refmap.t

  and 'a ref =
    | Ref : ('a option, 'a Refmap.t) field -> 'a ref
    | NullRef : 'a cls -> 'a ref

  type _ cls += RefMapCls : 'a cls -> 'a Refmap.t cls
  type _ cls += PolyType : 'a cls

  let rec string_of_typ : type a. prcls -> a typ -> string =
   fun {pr} t ->
    match t with
    | Basic Int -> "int"
    | Basic Int32 -> "int32"
    | Basic Int64 -> "int64"
    | Basic Bool -> "bool"
    | Basic Float -> "float"
    | Basic String -> "string"
    | Basic Char -> "char"
    | DateTime -> "datetime"
    | Array t -> Printf.sprintf "array(%s)" (string_of_typ {pr} t)
    | List x -> Printf.sprintf "list(%s)" (string_of_typ {pr} x)
    | Refv (cls, _typ) ->
        Printf.sprintf "refv(%s)"
          (match pr cls with Some x -> x | None -> "<unknown>")
    | Dict (basic, ty) ->
        Printf.sprintf "dict(%s,%s)"
          (string_of_typ {pr} (Basic basic))
          (string_of_typ {pr} ty)
    | Unit -> Printf.sprintf "unit"
    | Option x -> Printf.sprintf "option(%s)" (string_of_typ {pr} x)
    | Tuple (t1, t2) ->
        Printf.sprintf "(%s * %s)"
          (string_of_typ {pr} t1)
          (string_of_typ {pr} t2)
    | Tuple3 (t1, t2, t3) ->
        Printf.sprintf "(%s * %s * %s)"
          (string_of_typ {pr} t1)
          (string_of_typ {pr} t2)
          (string_of_typ {pr} t3)
    | Tuple4 (t1, t2, t3, t4) ->
        Printf.sprintf "(%s * %s * %s * %s)"
          (string_of_typ {pr} t1)
          (string_of_typ {pr} t2)
          (string_of_typ {pr} t3)
          (string_of_typ {pr} t4)
    | Struct s ->
        Printf.sprintf "struct(%s)"
          ( match List.hd s.fields with BoxedField fld -> (
              match pr fld.fcls with Some x -> x | None -> "<unknown>" ) )
    | Variant v ->
        Printf.sprintf "variant(%s)"
          ( match List.hd v.variants with BoxedTag t -> (
              match pr t.tcls with Some x -> x | None -> "<unknown>" ) )
    | Abstract a -> Printf.sprintf "abstract(%s)" a.aname
    | Refmap ty -> Printf.sprintf "refmap(%s)" (string_of_typ {pr} ty)

  let make_ref cls ty name =
    Ref
      { fget=
          (fun r ->
            try Some (Refmap.find name r) with Not_found ->
              Printf.printf "No ref for %s (refs are: [%s])\n%!" name
                (String.concat ";" (Refmap.keys r)) ;
              None )
      ; fset=
          (fun v r ->
            match v with
            | Some v -> Refmap.update name v r
            | None -> Refmap.remove name r )
      ; fname= [name]
      ; fdescription= ["Reference"]
      ; fdefault= None
      ; fcls= RefMapCls cls
      ; field= Option ty
      ; fversion= None }

  let compose : ('b, 'a) field -> ('c, 'b) field -> ('c, 'a) field =
   fun f1 f2 ->
    { fname= f1.fname @ f2.fname
    ; field= f2.field
    ; fget= (fun x -> f2.fget (f1.fget x))
    ; fset= (fun x r -> f1.fset (f2.fset x (f1.fget r)) r)
    ; fcls= f1.fcls
    ; fdefault= f2.fdefault
    ; fdescription= f2.fdescription
    ; fversion= f2.fversion }

  let opt_compose : ('b option, 'a) field -> ('c, 'b) field -> ('c, 'a) field =
   fun f1 f2 ->
    { f2 with
      fname= f1.fname @ f2.fname
    ; field= f2.field
    ; fget=
        (fun x ->
          match f1.fget x with
          | Some y -> f2.fget y
          | None ->
              failwith
                (Printf.sprintf "Missing row: %s" (String.concat "." f1.fname))
          )
    ; fset=
        (fun x r ->
          match f1.fget r with
          | Some y -> f1.fset (Some (f2.fset x y)) r
          | None -> r )
    ; fcls= f1.fcls }

  let name_of_ref = function
    | Ref x -> (
      match x.fname with [x] -> x | _ -> failwith "Invalid reference!" )
    | NullRef _ -> "OpaqueRef:NULL"

  let cls_of_ref = function
    | Ref x -> (
      match x.fcls with RefMapCls x -> x | _ -> failwith "Unknown cls" )
    | NullRef cls -> cls

  let rec eq_ty : type a b. a typ -> b typ -> (a, b) eq option =
   fun t1 t2 ->
    match (t1, t2) with
    | Basic Int, Basic Int -> Some Eq
    | Basic Int32, Basic Int32 -> Some Eq
    | Basic Int64, Basic Int64 -> Some Eq
    | Basic Bool, Basic Bool -> Some Eq
    | Basic Float, Basic Float -> Some Eq
    | Basic String, Basic String -> Some Eq
    | Basic Char, Basic Char -> Some Eq
    | DateTime, DateTime -> Some Eq
    | Array x, Array y -> (
      match eq_ty x y with Some Eq -> Some Eq | _ -> None )
    | List x, List y -> (
      match eq_ty x y with Some Eq -> Some Eq | _ -> None )
    | Refv (x, _), Refv (x', _) -> (
      match !eqcls.eq x x' with Some Eq -> Some Eq | _ -> None )
    | Dict (basic1, ty1), Dict (basic2, ty2) -> (
      match (eq_ty (Basic basic1) (Basic basic2), eq_ty ty1 ty2) with
      | Some Eq, Some Eq -> Some Eq
      | _, _ -> None )
    | Unit, Unit -> Some Eq
    | Option t1, Option t2 -> (
      match eq_ty t1 t2 with Some Eq -> Some Eq | _ -> None )
    | Tuple (t1, t2), Tuple (t1', t2') -> (
      match (eq_ty t1 t1', eq_ty t2 t2') with
      | Some Eq, Some Eq -> Some Eq
      | _ -> None )
    | Tuple3 (t1, t2, t3), Tuple3 (t1', t2', t3') -> (
      match (eq_ty t1 t1', eq_ty t2 t2', eq_ty t3 t3') with
      | Some Eq, Some Eq, Some Eq -> Some Eq
      | _ -> None )
    | Tuple4 (t1, t2, t3, t4), Tuple4 (t1', t2', t3', t4') -> (
      match (eq_ty t1 t1', eq_ty t2 t2', eq_ty t3 t3', eq_ty t4 t4') with
      | Some Eq, Some Eq, Some Eq, Some Eq -> Some Eq
      | _ -> None )
    | Struct s1, Struct s2 -> (
      match (List.hd s1.fields, List.hd s2.fields)
      with BoxedField f1, BoxedField f2 -> (
        match !eqcls.eq f1.fcls f2.fcls with Some Eq -> Some Eq | _ -> None ) )
    | Variant v1, Variant v2 -> (
      match (List.hd v1.variants, List.hd v2.variants)
      with BoxedTag t1, BoxedTag t2 -> (
        match !eqcls.eq t1.tcls t2.tcls with Some Eq -> Some Eq | _ -> None ) )
    | Abstract a1, Abstract a2 -> (
      match !eqcls.eq a1.acls a2.acls with Some Eq -> Some Eq | None -> None )
    | Refmap a1, Refmap a2 -> (
      match eq_ty a1 a2 with Some Eq -> Some Eq | None -> None )
    | _, _ -> None

  let eq_field : type a b c d.
      (a, b) field -> (c, d) field -> (a * b, c * d) eq option =
   fun f1 f2 ->
    match (!eqcls.eq f1.fcls f2.fcls, eq_ty f1.field f2.field) with
    | Some Eq, Some Eq -> Some Eq
    | _, _ -> None

  let int = {name= "int"; ty= Basic Int; description= ["Native integer"]}

  let int32 = {name= "int32"; ty= Basic Int32; description= ["32-bit integer"]}

  let int64 = {name= "int64"; ty= Basic Int64; description= ["64-bit integer"]}

  let bool = {name= "bool"; ty= Basic Bool; description= ["Boolean"]}

  let float =
    {name= "float"; ty= Basic Float; description= ["Floating-point number"]}

  let string = {name= "string"; ty= Basic String; description= ["String"]}

  let char = {name= "char"; ty= Basic Char; description= ["Char"]}

  let unit = {name= "unit"; ty= Unit; description= ["Unit"]}

  let default_types =
    [ BoxedDef int
    ; BoxedDef int32
    ; BoxedDef int64
    ; BoxedDef bool
    ; BoxedDef float
    ; BoxedDef string
    ; BoxedDef char
    ; BoxedDef unit ]
end

exception Runtime_error of string * t

exception Runtime_exception of string * string

let map_strings sep fn l = String.concat sep (List.map fn l)

let rec to_string t =
  let open Printf in
  match t with
  | Int i -> sprintf "I(%Li)" i
  | Int32 i -> sprintf "I32(%li)" i
  | Bool b -> sprintf "B(%b)" b
  | Float f -> sprintf "F(%g)" f
  | String s -> sprintf "S(%s)" s
  | DateTime s -> sprintf "D(%s)" s
  | Enum ts -> sprintf "[%s]" (map_strings ";" to_string ts)
  | Dict ts ->
      sprintf "{%s}"
        (map_strings ";" (fun (s, t) -> sprintf "%s:%s" s (to_string t)) ts)
  | Null -> "N"

let rpc_of_t x = x

let rpc_of_int64 i = Int i

let rpc_of_int32 i = Int (Int64.of_int32 i)

let rpc_of_int i = Int (Int64.of_int i)

let rpc_of_bool b = Bool b

let rpc_of_float f = Float f

let rpc_of_string s = String s

let rpc_of_dateTime s = DateTime s

let rpc_of_unit () = Null

let rpc_of_char x = Int (Int64.of_int (Char.code x))

let int64_of_rpc = function
  | Int i -> i
  | String s -> Int64.of_string s
  | x -> failwith (Printf.sprintf "Expected int64, got '%s'" (to_string x))

let int32_of_rpc = function
  | Int i -> Int64.to_int32 i
  | String s -> Int32.of_string s
  | x -> failwith (Printf.sprintf "Expected int32, got '%s'" (to_string x))

let int_of_rpc = function
  | Int i -> Int64.to_int i
  | String s -> int_of_string s
  | x -> failwith (Printf.sprintf "Expected int, got '%s'" (to_string x))

let bool_of_rpc = function
  | Bool b -> b
  | x -> failwith (Printf.sprintf "Expected bool, got '%s'" (to_string x))

let float_of_rpc = function
  | Float f -> f
  | Int i -> Int64.to_float i
  | Int32 i -> Int32.to_float i
  | String s -> float_of_string s
  | x -> failwith (Printf.sprintf "Expected float, got '%s'" (to_string x))

let string_of_rpc = function
  | String s -> s
  | x -> failwith (Printf.sprintf "Expected string, got '%s'" (to_string x))

let dateTime_of_rpc = function
  | DateTime s -> s
  | x -> failwith (Printf.sprintf "Expected DateTime, got '%s'" (to_string x))

let unit_of_rpc = function
  | Null -> ()
  | x -> failwith (Printf.sprintf "Expected unit, got '%s'" (to_string x))

let char_of_rpc x =
  let x = int_of_rpc x in
  if x < 0 || x > 255 then failwith (Printf.sprintf "Char out of range (%d)" x)
  else Char.chr x

let t_of_rpc t = t

let lowerfn = function
  | String s -> String (String.lowercase_ascii s)
  | Enum (String s :: ss) -> Enum (String (String.lowercase_ascii s) :: ss)
  | x -> x

module ResultUnmarshallers = struct
  open Rresult

  let int64_of_rpc = function
    | Int i -> R.ok i
    | String s -> (
      try R.ok (Int64.of_string s) with _ ->
        R.error_msg (Printf.sprintf "Expected int64, got string '%s'" s) )
    | x ->
        R.error_msg (Printf.sprintf "Expected int64, got '%s'" (to_string x))

  let int32_of_rpc = function
    | Int i -> R.ok (Int64.to_int32 i)
    | String s -> (
      try R.ok (Int32.of_string s) with _ ->
        R.error_msg (Printf.sprintf "Expected int32, got string '%s'" s) )
    | x ->
        R.error_msg (Printf.sprintf "Expected int32, got '%s'" (to_string x))

  let int_of_rpc = function
    | Int i -> R.ok (Int64.to_int i)
    | String s -> (
      try R.ok (int_of_string s) with _ ->
        R.error_msg (Printf.sprintf "Expected int, got string '%s'" s) )
    | x -> R.error_msg (Printf.sprintf "Expected int, got '%s'" (to_string x))

  let bool_of_rpc = function
    | Bool b -> R.ok b
    | x -> R.error_msg (Printf.sprintf "Expected bool, got '%s'" (to_string x))

  let float_of_rpc = function
    | Float f -> R.ok f
    | Int i -> R.ok (Int64.to_float i)
    | Int32 i -> R.ok (Int32.to_float i)
    | String s -> (
      try R.ok (float_of_string s) with _ ->
        R.error_msg (Printf.sprintf "Expected float, got string '%s'" s) )
    | x ->
        R.error_msg (Printf.sprintf "Expected float, got '%s'" (to_string x))

  let string_of_rpc = function
    | String s -> R.ok s
    | x ->
        R.error_msg (Printf.sprintf "Expected string, got '%s'" (to_string x))

  let dateTime_of_rpc = function
    | DateTime s -> R.ok s
    | x ->
        R.error_msg
          (Printf.sprintf "Expected DateTime, got '%s'" (to_string x))

  let unit_of_rpc = function
    | Null -> R.ok ()
    | x -> R.error_msg (Printf.sprintf "Expected unit, got '%s'" (to_string x))

  let char_of_rpc x =
    Rresult.R.bind (int_of_rpc x) (fun x ->
        if x < 0 || x > 255 then
          R.error_msg (Printf.sprintf "Char out of range (%d)" x)
        else R.ok (Char.chr x) )

  let t_of_rpc t = R.ok t
end

let struct_extend rpc default_rpc =
  match (rpc, default_rpc) with
  | Dict real, Dict default_fields ->
      Dict
        (List.fold_left
           (fun real (f, default) ->
             if List.mem_assoc f real then real else (f, default) :: real )
           real default_fields)
  | _, _ -> rpc

type callback = string list -> t -> unit

type call = {name: string; params: t list}

let call name params = {name; params}

let string_of_call call =
  Printf.sprintf "-> %s(%s)" call.name
    (String.concat "," (List.map to_string call.params))

type response = {success: bool; contents: t}

let string_of_response response =
  Printf.sprintf "<- %s(%s)"
    (if response.success then "success" else "failure")
    (to_string response.contents)

let success v = {success= true; contents= v}

let failure v = {success= false; contents= v}
