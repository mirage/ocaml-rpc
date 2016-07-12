open Rpc.Types
open Idl
open Codegen
    
let rec string_of_t : type a.a typ -> string list =
  let of_basic : type b.b basic -> string = function
    | Int -> "int"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Bool -> "bool"
    | Float -> "float"
    | String -> "string"
    | Char -> "char"
  in
  let print txt = [ txt ] in
  function
  | Basic b -> print (of_basic b)
  | DateTime -> print (of_basic String)
  | Struct _ -> print ("struct  { ... }")
  | Variant _ -> print ("variant { ... }")
  | Array t -> string_of_t t @ (print " list")
  | List t -> string_of_t t @ (print " list")
  | Dict (key, v) -> print (Printf.sprintf "(%s * " (of_basic key)) @ (string_of_t v) @ (print ") list");
  | Unit -> print "unit"
  | Option x -> string_of_t x @ (print " option")
  | Tuple (a, b) -> string_of_t a @ (print " * ") @ (string_of_t b)

let table headings rows =
  (* Slightly more convenient to have columns sometimes. This
     also ensures each row has the correct number of entries. *)
  let transpose mat =
    let rec inner r =
      let safe_hd = function | hd::tl -> hd | _ -> "" in
      let safe_tl = function | hd::tl -> tl | _ -> [] in
      match r with
      | ([])::_ -> []
      | _ -> List.(map safe_hd r :: inner (map safe_tl r))
    in
    inner mat
  in

  let columns = transpose (headings::rows) in
  let all_rows = transpose columns in
  
  let col_widths =
    let col_width col = List.fold_left max 0 col in
    let widths = List.map String.length in
    List.map (fun col -> col_width (widths col)) columns
  in

  let pad c n s = Printf.sprintf "%s%s" s (String.make (n - String.length s) c) in
  let padfns = List.map (pad ' ') col_widths in
  
  let pad_row row = List.map2 (fun fn x -> fn x) padfns row in
  let padded = List.map pad_row all_rows in

  let row_to_string row = Printf.sprintf "| %s |" (String.concat " | " (row)) in
  let separator = Printf.sprintf "|-%s-|" (String.concat "-|-" (List.map (fun width -> pad '-' width "") col_widths)) in
  row_to_string (List.hd padded) :: separator :: (List.map row_to_string (List.tl padded))

let link uri text =
  Printf.sprintf "[%s](%s)" text uri

let h1 txt = [ txt; String.make (String.length txt) '=' ]
let h2 txt = [ txt; String.make (String.length txt) '-' ]
let h3 txt = [ Printf.sprintf "### %s" txt ]
let h4 txt = [ Printf.sprintf "#### %s" txt ]
let h5 txt = [ Printf.sprintf "##### %s" txt ]
let h6 txt = [ Printf.sprintf "###### %s" txt ]
let hrule = [ "---" ]
  
(* Function inputs and outputs in a table *)
let of_args args =
  let row_of_arg (is_in, Param.Boxed arg) =
    let name = arg.Param.name in
    let direction = if is_in then "in" else "out" in
    let ty = string_of_t arg.Param.typedef.ty |> String.concat " " in
    let description = arg.Param.description in
    [name; direction; ty; description]
  in
  table ["Name"; "Direction"; "Type"; "Description"] (List.map row_of_arg args)

let of_struct_fields : 'a boxed_field list -> string list = fun all ->
  let of_row (BoxedField f) =
    let ty = string_of_t f.field in
    [f.fname; String.concat "" ty; f.fdescription]
  in
  table ["Name"; "Type"; "Description"] (List.map of_row all)

let of_variant_tags : 'a boxed_tag list -> string list = fun all ->
  let of_row (BoxedTag t) =
    let ty = string_of_t t.vcontents in    
    [t.vname; String.concat "" ty; t.vdescription]
  in
  table ["Name"; "Type"; "Description"] (List.map of_row all)

let of_type_decl i_opt (BoxedDef t) =
  let name = t.name in
  let defn = String.concat "" (string_of_t t.ty) in
  let description = t.description in
  let common =
    [ Printf.sprintf "### type `%s`" name;
      Printf.sprintf "type %s = %s" name defn;
      description ]
  in
  let rest = match t.ty with
    | Struct structure -> "Members:" :: of_struct_fields structure.fields
    | Variant variant -> "Constructors:" :: of_variant_tags variant.variants
    | _ -> [] in
  common @ rest

let of_method is i (Codegen.BoxedFunction m) =
  let name = m.Method.name in
  let description = m.Method.description in
  h3 name @ [ description ] @ [ (* tabs_of *) ]

let of_interface is i =
  let name = i.Interface.details.Idl.Interface.name in
  let description = i.Interface.details.Idl.Interface.description in
  h2 name @ [ description ] @ List.concat (List.map (of_method is i) i.Interface.methods)
    
let of_interfaces x =
  let name = x.Interfaces.name in
  let description = x.Interfaces.description in
  h1 name @
  [ description ] @
  List.concat (List.map (of_type_decl None) x.Interfaces.type_decls) @
  List.concat (List.map (of_interface x) x.Interfaces.interfaces)
    
let to_string x = String.concat "\n" (of_interfaces x)



