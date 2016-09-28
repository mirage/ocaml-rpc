open OUnit
let string_of_err = function | `Msg x -> x
let rec canonicalise r =
  match r with
  | Rpc.Dict x ->
      Rpc.Dict
        ((List.sort (fun (s1,_)  -> fun (s2,_)  -> compare s1 s2) x) |>
           (List.map (fun (x,y)  -> (x, (canonicalise y)))))
  | Rpc.Enum ys -> Rpc.Enum (List.map canonicalise ys)
  | x -> x
let check_marshal_unmarshal: ('a* Rpc.t* 'a Rpc.Types.typ) -> unit =
  fun (x,r,typ)  ->
    let r' = Rpcmarshal.marshal typ x in
    let x' = Rpcmarshal.unmarshal typ r in
    (match x' with
     | Result.Error e ->
         Printf.printf "\nFound Error when expecting OK: %s\n%!"
           (string_of_err e)
     | Result.Ok y ->
         Printf.printf "\nOK: (%s)\n%!"
           ((Rpcmarshal.marshal typ y) |> Rpc.to_string));
    assert_equal (Result.Ok x) x';
    if (canonicalise r) <> (canonicalise r')
    then
      Printf.printf "\nmarshalled stuff is different: r=%s r'=%s\n%!"
        (Rpc.to_string r) (Rpc.to_string r');
    assert_equal (canonicalise r) (canonicalise r')
let check_unmarshal_error: 'a Rpc.Types.typ -> Rpc.t -> unit =
  fun typ  ->
    fun t  ->
      match Rpcmarshal.unmarshal typ t with
      | Result.Ok _ -> assert_equal false true
      | Result.Error e -> Printf.printf "%s\n" (string_of_err e)
let check_unmarshal_ok: 'a -> 'a Rpc.Types.typ -> Rpc.t -> unit =
  fun x  ->
    fun typ  ->
      fun r  ->
        match Rpcmarshal.unmarshal typ r with
        | Result.Ok x' -> assert_equal x x'
        | Result.Error _ -> assert_equal false true
type test_int = int[@@deriving rpcty]
let rec typ_of_test_int = let open! Rpc.Types in Basic Int
and test_int_def =
  let open! Rpc.Types in
    { name = "test_int"; description = ""; ty = typ_of_test_int }
let test_int () = check_marshal_unmarshal (1, (Rpc.Int 1L), typ_of_test_int)
let test_int_from_string () =
  check_unmarshal_ok 1 typ_of_test_int (Rpc.String "1")
let test_bad_int () = check_unmarshal_error typ_of_test_int Rpc.Null
let test_bad_int_string () =
  check_unmarshal_error typ_of_test_int (Rpc.String "tree")
type test_int32 = int32[@@deriving rpcty]
let rec typ_of_test_int32 = let open! Rpc.Types in Basic Int32
and test_int32_def =
  let open! Rpc.Types in
    { name = "test_int32"; description = ""; ty = typ_of_test_int32 }
let test_int32 () =
  check_marshal_unmarshal (1l, (Rpc.Int 1L), typ_of_test_int32)
let test_int32_from_string () =
  check_unmarshal_ok 1l typ_of_test_int32 (Rpc.String "1")
let test_bad_int32 () = check_unmarshal_error typ_of_test_int32 Rpc.Null
let test_bad_int32_string () =
  check_unmarshal_error typ_of_test_int32 (Rpc.String "tree")
type test_int64 = int64[@@deriving rpcty]
let rec typ_of_test_int64 = let open! Rpc.Types in Basic Int64
and test_int64_def =
  let open! Rpc.Types in
    { name = "test_int64"; description = ""; ty = typ_of_test_int64 }
let test_int64 () =
  check_marshal_unmarshal (1L, (Rpc.Int 1L), typ_of_test_int64)
let test_int64_from_string () =
  check_unmarshal_ok 1L typ_of_test_int64 (Rpc.String "1")
let test_bad_int64 () = check_unmarshal_error typ_of_test_int64 Rpc.Null
let test_bad_int64_string () =
  check_unmarshal_error typ_of_test_int64 (Rpc.String "tree")
type test_unit = unit[@@deriving rpcty]
let rec typ_of_test_unit = let open! Rpc.Types in Unit
and test_unit_def =
  let open! Rpc.Types in
    { name = "test_unit"; description = ""; ty = typ_of_test_unit }
let test_unit () = check_marshal_unmarshal ((), Rpc.Null, typ_of_test_unit)
let test_bad_unit () = check_unmarshal_error typ_of_test_unit (Rpc.Int 1L)
type test_string = string[@@deriving rpcty]
let rec typ_of_test_string = let open! Rpc.Types in Basic String
and test_string_def =
  let open! Rpc.Types in
    { name = "test_string"; description = ""; ty = typ_of_test_string }
let test_string () =
  check_marshal_unmarshal
    ("test string", (Rpc.String "test string"), typ_of_test_string)
let test_bad_string () =
  check_unmarshal_error typ_of_test_string (Rpc.Int 1L)
type test_float = float[@@deriving rpcty]
let rec typ_of_test_float = let open! Rpc.Types in Basic Float
and test_float_def =
  let open! Rpc.Types in
    { name = "test_float"; description = ""; ty = typ_of_test_float }
let test_float () =
  check_marshal_unmarshal (2.0, (Rpc.Float 2.0), typ_of_test_float)
let test_float_from_string () =
  check_unmarshal_ok 1.0 typ_of_test_float (Rpc.String "1.0")
let test_bad_float () = check_unmarshal_error typ_of_test_float (Rpc.Enum [])
let test_bad_float_string () =
  check_unmarshal_error typ_of_test_float (Rpc.String "xxy")
type test_bool = bool[@@deriving rpcty]
let rec typ_of_test_bool = let open! Rpc.Types in Basic Bool
and test_bool_def =
  let open! Rpc.Types in
    { name = "test_bool"; description = ""; ty = typ_of_test_bool }
let test_bool () =
  check_marshal_unmarshal (true, (Rpc.Bool true), typ_of_test_bool)
let test_bad_bool () =
  check_unmarshal_error typ_of_test_bool (Rpc.String "true")
type test_char = char[@@deriving rpcty]
let rec typ_of_test_char = let open! Rpc.Types in Basic Char
and test_char_def =
  let open! Rpc.Types in
    { name = "test_char"; description = ""; ty = typ_of_test_char }
let test_char () =
  check_marshal_unmarshal
    ('x', (Rpc.Int ((Char.code 'x') |> Int64.of_int)), typ_of_test_char)
let test_bad_char () =
  check_unmarshal_error typ_of_test_char (Rpc.String "x")
type test_int_list = int list[@@deriving rpcty]
let rec typ_of_test_int_list =
  let open! Rpc.Types in List (let open! Rpc.Types in Basic Int)
and test_int_list_def =
  let open! Rpc.Types in
    { name = "test_int_list"; description = ""; ty = typ_of_test_int_list }
let test_int_list () =
  check_marshal_unmarshal
    ([1; 2; 3; 4],
      (Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L; Rpc.Int 4L]),
      typ_of_test_int_list)
type test_int_array = int array[@@deriving rpcty]
let rec typ_of_test_int_array =
  let open! Rpc.Types in Array (let open! Rpc.Types in Basic Int)
and test_int_array_def =
  let open! Rpc.Types in
    { name = "test_int_array"; description = ""; ty = typ_of_test_int_array }
let test_int_array () =
  check_marshal_unmarshal
    ([|1;2;3;4|],
      (Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L; Rpc.Int 4L]),
      typ_of_test_int_array)
type test_tuple2 = (int* string)[@@deriving rpcty]
let rec typ_of_test_tuple2 =
  let open! Rpc.Types in
    Tuple
      ((let open! Rpc.Types in Basic Int),
        (let open! Rpc.Types in Basic String))
and test_tuple2_def =
  let open! Rpc.Types in
    { name = "test_tuple2"; description = ""; ty = typ_of_test_tuple2 }
let test_tuple2 () =
  check_marshal_unmarshal
    ((3, "hello"), (Rpc.Enum [Rpc.Int 3L; Rpc.String "hello"]),
      typ_of_test_tuple2)
type test_option = int option[@@deriving rpcty]
let rec typ_of_test_option =
  let open! Rpc.Types in Option (let open! Rpc.Types in Basic Int)
and test_option_def =
  let open! Rpc.Types in
    { name = "test_option"; description = ""; ty = typ_of_test_option }
let test_option () =
  check_marshal_unmarshal
    ((Some 1), (Rpc.Enum [Rpc.Int 1L]), typ_of_test_option)
let test_option_none () =
  check_marshal_unmarshal (None, (Rpc.Enum []), typ_of_test_option)
let test_bad_option () =
  check_unmarshal_error typ_of_test_option (Rpc.Int 5L)
type test_constr = test_int[@@deriving rpcty]
let rec typ_of_test_constr = let open! Rpc.Types in typ_of_test_int
and test_constr_def =
  let open! Rpc.Types in
    { name = "test_constr"; description = ""; ty = typ_of_test_constr }
let test_constr () =
  check_marshal_unmarshal (1, (Rpc.Int 1L), typ_of_test_constr)
type test_variant =
  | VNone
  | VOne of int
  | VTwo of (int* int)[@@deriving rpcty]
let rec typ_of_test_variant =
  let open! Rpc.Types in
    Variant
      ({
         variants =
           [BoxedTag
              {
                vname = "VNone";
                vcontents = Unit;
                vdescription = "";
                vpreview = ((function | VNone  -> Some () | _ -> None));
                vreview = ((function | () -> VNone))
              };
           BoxedTag
             {
               vname = "VOne";
               vcontents = (let open! Rpc.Types in Basic Int);
               vdescription = "";
               vpreview = ((function | VOne a0 -> Some a0 | _ -> None));
               vreview = ((function | a0 -> VOne a0))
             };
           BoxedTag
             {
               vname = "VTwo";
               vcontents =
                 (let open! Rpc.Types in
                    Tuple
                      ((let open! Rpc.Types in Basic Int),
                        (let open! Rpc.Types in Basic Int)));
               vdescription = "";
               vpreview = ((function | VTwo a0 -> Some a0 | _ -> None));
               vreview = ((function | a0 -> VTwo a0))
             }];
         vconstructor =
           (fun s'  ->
              fun t  ->
                let s = String.lowercase s' in
                match s with
                | "vnone" ->
                    Rresult.R.bind (t.t Unit)
                      (function | () -> Rresult.R.ok VNone)
                | "vone" ->
                    Rresult.R.bind (t.t (let open! Rpc.Types in Basic Int))
                      (function | a0 -> Rresult.R.ok (VOne a0))
                | "vtwo" ->
                    Rresult.R.bind
                      (t.t
                         (let open! Rpc.Types in
                            Tuple
                              ((let open! Rpc.Types in Basic Int),
                                (let open! Rpc.Types in Basic Int))))
                      (function | a0 -> Rresult.R.ok (VTwo a0))
                | _ ->
                    Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s))
       } : test_variant variant)
and test_variant_def =
  let open! Rpc.Types in
    { name = "test_variant"; description = ""; ty = typ_of_test_variant }
let test_variant () =
  check_marshal_unmarshal (VNone, (Rpc.String "VNone"), typ_of_test_variant)
let test_variant1 () =
  check_marshal_unmarshal
    ((VOne 1), (Rpc.Enum [Rpc.String "VOne"; Rpc.Int 1L]),
      typ_of_test_variant)
let test_variant2 () =
  check_marshal_unmarshal
    ((VTwo (1, 2)),
      (Rpc.Enum [Rpc.String "VTwo"; Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L]]),
      typ_of_test_variant)
let test_variant_case () =
  check_unmarshal_ok VNone typ_of_test_variant (Rpc.String "vnone")
let test_bad_variant_case () =
  check_unmarshal_error typ_of_test_variant
    (Rpc.Enum [Rpc.String "vtwo"; Rpc.Int 5L])
type test_variant_name =
  | VThree of int[@name "bob"]
  | VFour[@name "lofty"][@@deriving rpcty]
let rec typ_of_test_variant_name =
  let open! Rpc.Types in
    Variant
      ({
         variants =
           [BoxedTag
              {
                vname = "bob";
                vcontents = (let open! Rpc.Types in Basic Int);
                vdescription = "";
                vpreview = ((function | VThree a0 -> Some a0 | _ -> None));
                vreview = ((function | a0 -> VThree a0))
              };
           BoxedTag
             {
               vname = "lofty";
               vcontents = Unit;
               vdescription = "";
               vpreview = ((function | VFour  -> Some () | _ -> None));
               vreview = ((function | () -> VFour))
             }];
         vconstructor =
           (fun s'  ->
              fun t  ->
                let s = String.lowercase s' in
                match s with
                | "bob" ->
                    Rresult.R.bind (t.t (let open! Rpc.Types in Basic Int))
                      (function | a0 -> Rresult.R.ok (VThree a0))
                | "lofty" ->
                    Rresult.R.bind (t.t Unit)
                      (function | () -> Rresult.R.ok VFour)
                | _ ->
                    Rresult.R.error_msg (Printf.sprintf "Unknown tag '%s'" s))
       } : test_variant_name variant)
and test_variant_name_def =
  let open! Rpc.Types in
    {
      name = "test_variant_name";
      description = "";
      ty = typ_of_test_variant_name
    }
let test_variant_name () =
  check_marshal_unmarshal
    ((VThree 5), (Rpc.Enum [Rpc.String "bob"; Rpc.Int 5L]),
      typ_of_test_variant_name)
let test_variant_name2 () =
  check_marshal_unmarshal
    (VFour, (Rpc.String "lofty"), typ_of_test_variant_name)
type test_record = {
  fiEld1: int;
  fiEld2: string;}[@@deriving rpcty]
let rec (test_record_fiEld1 :(_,test_record) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "fiEld1";
      field = (let open! Rpc.Types in Basic Int);
      fdescription = "";
      fget = (fun _r  -> _r.fiEld1);
      fset = (fun v  -> fun s  -> { s with fiEld1 = v })
    }
and (test_record_fiEld2 :(_,test_record) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "fiEld2";
      field = (let open! Rpc.Types in Basic String);
      fdescription = "";
      fget = (fun _r  -> _r.fiEld2);
      fset = (fun v  -> fun s  -> { s with fiEld2 = v })
    }
and test_record =
  let open Rpc.Types in
    ({
       fields =
         [BoxedField test_record_fiEld1; BoxedField test_record_fiEld2];
       sname = "test_record";
       constructor =
         (fun getter  ->
            let open Rresult.R in
              (getter.Rpc.Types.g "fiEld2"
                 (let open! Rpc.Types in Basic String))
                >>=
                (fun test_record_fiEld2  ->
                   (getter.Rpc.Types.g "fiEld1"
                      (let open! Rpc.Types in Basic Int))
                     >>=
                     (fun test_record_fiEld1  ->
                        return
                          {
                            fiEld1 = test_record_fiEld1;
                            fiEld2 = test_record_fiEld2
                          })))
     } : test_record Rpc.Types.structure)
and typ_of_test_record = Rpc.Types.Struct test_record
and test_record_def =
  let open! Rpc.Types in
    { name = "test_record"; description = ""; ty = typ_of_test_record }
let test_record () =
  check_marshal_unmarshal
    ({ fiEld1 = 7; fiEld2 = "banana" },
      (Rpc.Dict [("fiEld1", (Rpc.Int 7L)); ("fiEld2", (Rpc.String "banana"))]),
      typ_of_test_record)
let test_record_case () =
  check_unmarshal_ok { fiEld1 = 7; fiEld2 = "banana" } typ_of_test_record
    (Rpc.Dict [("field1", (Rpc.Int 7L)); ("FIELD2", (Rpc.String "banana"))])
let test_bad_record () =
  check_unmarshal_error typ_of_test_record
    (Rpc.Dict [("field1", (Rpc.Int 7L))])
type test_record_opt = {
  field3: int option;
  field4: string option;}[@@deriving rpcty]
let rec (test_record_opt_field3 :(_,test_record_opt) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "field3";
      field =
        (let open! Rpc.Types in Option (let open! Rpc.Types in Basic Int));
      fdescription = "";
      fget = (fun _r  -> _r.field3);
      fset = (fun v  -> fun s  -> { s with field3 = v })
    }
and (test_record_opt_field4 :(_,test_record_opt) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "field4";
      field =
        (let open! Rpc.Types in Option (let open! Rpc.Types in Basic String));
      fdescription = "";
      fget = (fun _r  -> _r.field4);
      fset = (fun v  -> fun s  -> { s with field4 = v })
    }
and test_record_opt =
  let open Rpc.Types in
    ({
       fields =
         [BoxedField test_record_opt_field3;
         BoxedField test_record_opt_field4];
       sname = "test_record_opt";
       constructor =
         (fun getter  ->
            let open Rresult.R in
              (getter.Rpc.Types.g "field4"
                 (let open! Rpc.Types in
                    Option (let open! Rpc.Types in Basic String)))
                >>=
                (fun test_record_opt_field4  ->
                   (getter.Rpc.Types.g "field3"
                      (let open! Rpc.Types in
                         Option (let open! Rpc.Types in Basic Int)))
                     >>=
                     (fun test_record_opt_field3  ->
                        return
                          {
                            field3 = test_record_opt_field3;
                            field4 = test_record_opt_field4
                          })))
     } : test_record_opt Rpc.Types.structure)
and typ_of_test_record_opt = Rpc.Types.Struct test_record_opt
and test_record_opt_def =
  let open! Rpc.Types in
    { name = "test_record_opt"; description = ""; ty = typ_of_test_record_opt
    }
let test_record_opt1 () =
  check_marshal_unmarshal
    ({ field3 = (Some 7); field4 = (Some "banana") },
      (Rpc.Dict [("field3", (Rpc.Int 7L)); ("field4", (Rpc.String "banana"))]),
      typ_of_test_record_opt)
let test_record_opt2 () =
  check_marshal_unmarshal
    ({ field3 = (Some 7); field4 = None },
      (Rpc.Dict [("field3", (Rpc.Int 7L))]), typ_of_test_record_opt)
let test_record_opt3 () =
  check_marshal_unmarshal
    ({ field3 = None; field4 = (Some "hamster") },
      (Rpc.Dict [("field4", (Rpc.String "hamster"))]),
      typ_of_test_record_opt)
let test_record_opt4 () =
  check_marshal_unmarshal
    ({ field3 = None; field4 = None }, (Rpc.Dict []), typ_of_test_record_opt)
type test_record_attrs = {
  field5: int[@key "foo"];}[@@deriving rpcty]
let rec (test_record_attrs_field5 :(_,test_record_attrs) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "foo";
      field = (let open! Rpc.Types in Basic Int);
      fdescription = "";
      fget = (fun _r  -> _r.field5);
      fset = (fun v  -> fun s  -> { s with field5 = v })
    }
and test_record_attrs =
  let open Rpc.Types in
    ({
       fields = [BoxedField test_record_attrs_field5];
       sname = "test_record_attrs";
       constructor =
         (fun getter  ->
            let open Rresult.R in
              (getter.Rpc.Types.g "foo" (let open! Rpc.Types in Basic Int))
                >>=
                (fun test_record_attrs_field5  ->
                   return { field5 = test_record_attrs_field5 }))
     } : test_record_attrs Rpc.Types.structure)
and typ_of_test_record_attrs = Rpc.Types.Struct test_record_attrs
and test_record_attrs_def =
  let open! Rpc.Types in
    {
      name = "test_record_attrs";
      description = "";
      ty = typ_of_test_record_attrs
    }
let test_record_attrs () =
  check_marshal_unmarshal
    ({ field5 = 6 }, (Rpc.Dict [("foo", (Rpc.Int 6L))]),
      typ_of_test_record_attrs)
type 'a test_poly = 'a list[@@deriving rpcty]
let rec typ_of_test_poly poly_a =
  let open! Rpc.Types in List (let open! Rpc.Types in poly_a)
and test_poly_def =
  let open! Rpc.Types in
    fun poly_a  ->
      { name = "test_poly"; description = ""; ty = (typ_of_test_poly poly_a)
      }
let test_poly () =
  let (x :int test_poly)= [1; 2; 3] in
  check_marshal_unmarshal
    (x, (Rpc.Enum [Rpc.Int 1L; Rpc.Int 2L; Rpc.Int 3L]),
      (typ_of_test_poly (Rpc.Types.Basic Rpc.Types.Int)))
type nested =
  {
  variant: test_variant_name;
  var2: test_variant;
  record: test_record_opt;
  rec2: test_record;}[@@deriving rpcty]
let rec (nested_variant :(_,nested) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "variant";
      field = (let open! Rpc.Types in typ_of_test_variant_name);
      fdescription = "";
      fget = (fun _r  -> _r.variant);
      fset = (fun v  -> fun s  -> { s with variant = v })
    }
and (nested_var2 :(_,nested) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "var2";
      field = (let open! Rpc.Types in typ_of_test_variant);
      fdescription = "";
      fget = (fun _r  -> _r.var2);
      fset = (fun v  -> fun s  -> { s with var2 = v })
    }
and (nested_record :(_,nested) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "record";
      field = (let open! Rpc.Types in typ_of_test_record_opt);
      fdescription = "";
      fget = (fun _r  -> _r.record);
      fset = (fun v  -> fun s  -> { s with record = v })
    }
and (nested_rec2 :(_,nested) Rpc.Types.field)=
  let open Rpc.Types in
    {
      fname = "rec2";
      field = (let open! Rpc.Types in typ_of_test_record);
      fdescription = "";
      fget = (fun _r  -> _r.rec2);
      fset = (fun v  -> fun s  -> { s with rec2 = v })
    }
and nested =
  let open Rpc.Types in
    ({
       fields =
         [BoxedField nested_variant;
         BoxedField nested_var2;
         BoxedField nested_record;
         BoxedField nested_rec2];
       sname = "nested";
       constructor =
         (fun getter  ->
            let open Rresult.R in
              (getter.Rpc.Types.g "rec2"
                 (let open! Rpc.Types in typ_of_test_record))
                >>=
                (fun nested_rec2  ->
                   (getter.Rpc.Types.g "record"
                      (let open! Rpc.Types in typ_of_test_record_opt))
                     >>=
                     (fun nested_record  ->
                        (getter.Rpc.Types.g "var2"
                           (let open! Rpc.Types in typ_of_test_variant))
                          >>=
                          (fun nested_var2  ->
                             (getter.Rpc.Types.g "variant"
                                (let open! Rpc.Types in
                                   typ_of_test_variant_name))
                               >>=
                               (fun nested_variant  ->
                                  return
                                    {
                                      variant = nested_variant;
                                      var2 = nested_var2;
                                      record = nested_record;
                                      rec2 = nested_rec2
                                    })))))
     } : nested Rpc.Types.structure)
and typ_of_nested = Rpc.Types.Struct nested
and nested_def =
  let open! Rpc.Types in
    { name = "nested"; description = ""; ty = typ_of_nested }
let fakegen () =
  let fake ty =
    let fake = Rpc_genfake.genall ty in
    let ss =
      List.map (fun f  -> (Rpcmarshal.marshal ty f) |> Jsonrpc.to_string)
        fake in
    let test2 =
      List.map
        (fun json  -> Rpcmarshal.unmarshal ty (Jsonrpc.of_string json)) ss in
    List.iter2
      (function
       | a ->
           (function
            | Result.Ok b -> (assert (a = b); ())
            | _ -> assert false)) fake test2;
    List.iter (fun s  -> Printf.printf "%s\n" s) ss in
  fake typ_of_test_record_opt;
  fake typ_of_test_variant_name;
  fake typ_of_nested
let suite =
  "basic_tests" >:::
    ["int" >:: test_int;
    "int_from_string" >:: test_int_from_string;
    "bad_int" >:: test_bad_int;
    "bad_int_string" >:: test_bad_int_string;
    "int32" >:: test_int32;
    "int32_from_string" >:: test_int32_from_string;
    "bad_int32" >:: test_bad_int32;
    "bad_int32_string" >:: test_bad_int32_string;
    "int64" >:: test_int64;
    "int64_from_string" >:: test_int64_from_string;
    "bad_int64" >:: test_bad_int64;
    "bad_int64_string" >:: test_bad_int64_string;
    "unit" >:: test_unit;
    "bad_unit" >:: test_bad_unit;
    "string" >:: test_string;
    "bad_string" >:: test_bad_string;
    "float" >:: test_float;
    "float_from_string" >:: test_float_from_string;
    "bad_float" >:: test_bad_float;
    "bad_float_string" >:: test_bad_float_string;
    "bool" >:: test_bool;
    "bad_bool" >:: test_bad_bool;
    "char" >:: test_char;
    "bad_char" >:: test_bad_char;
    "int list" >:: test_int_list;
    "int array" >:: test_int_array;
    "tuple2" >:: test_tuple2;
    "option" >:: test_option;
    "option (none)" >:: test_option_none;
    "bad_option" >:: test_bad_option;
    "constr" >:: test_constr;
    "variant" >:: test_variant;
    "variant1" >:: test_variant1;
    "variant2" >:: test_variant2;
    "variant_case" >:: test_variant_case;
    "variant_name" >:: test_variant_name;
    "variant_name2" >:: test_variant_name2;
    "bad_variant_case" >:: test_bad_variant_case;
    "record" >:: test_record;
    "record_case" >:: test_record_case;
    "bad_record" >:: test_bad_record;
    "record_opt1" >:: test_record_opt1;
    "record_opt2" >:: test_record_opt2;
    "record_opt3" >:: test_record_opt3;
    "record_opt4" >:: test_record_opt4;
    "record_attrs" >:: test_record_attrs;
    "poly" >:: test_poly;
    "fakegen" >:: fakegen]
let _ =
  Random.self_init ();
  (let results = run_test_tt_main suite in
   if List.exists (function | RSuccess _ -> false | _ -> true) results
   then exit 1)
File "tytest.ml", line 143, characters 0-75:
Warning 23: all the fields are explicitly listed in this record:
the 'with' clause is useless.
