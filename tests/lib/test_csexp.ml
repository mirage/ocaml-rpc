(*
 * Copyright (c) Cloud Software Group, Inc.
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

open Rpc

let string_all = String.init 255 Char.chr

let tests =
  [ "(3:Int20:-9223372036854775808)", Int Int64.min_int
  ; "(3:Int19:9223372036854775807)", Int Int64.max_int
  ; "(4:Bool4:true)", Bool true
  ; "(4:Bool5:false)", Bool false
  ; "(5:Float23:2.2250738585072014E-308)", Float Float.min_float
  ; "(5:Float23:1.7976931348623157E+308)", Float Float.max_float
  ; "(5:Float3:INF)", Float Float.infinity
  ; "(5:Float3:NAN)", Float Float.nan
  ; "(5:Float4:-INF)", Float Float.neg_infinity
  ; "(5:Float22:2.2204460492503131E-16)", Float Float.epsilon
  ; "(6:String0:)", String ""
  ; "(6:String6:O\000K\226\156\147)", String "O\000K\u{2713}"
  ; "(6:String255:" ^ string_all ^ ")", String string_all
  ; "(8:DateTime18:19700101T00:00:00Z)", DateTime "19700101T00:00:00Z"
  ; "(4:Enum())", Enum []
  ; ( "(4:Enum((3:Int1:0)(4:Bool4:true)(5:Float1:0)(6:String3:foo)(4:Enum((4:Bool5:false)))(4:Dict((1:a4:Null)))(6:Base640:)4:Null))"
    , Enum
        [ Int 0L
        ; Bool true
        ; Float 0.
        ; String "foo"
        ; Enum [ Bool false ]
        ; Dict [ "a", Null ]
        ; Base64 ""
        ; Null
        ] )
  ; "(4:Dict())", Dict []
  ; "(4:Dict((1:a(6:String1:b))))", Dict [ "a", String "b" ]
  ; "(6:Base64255:" ^ string_all ^ ")", Base64 string_all
  ; "4:Null", Null
  ]


open Alcotest.V1

let test_roundtrip kind tests testable to_string of_string =
  let test_encode (expected, input) () =
    let actual = to_string input in
    check' ~msg:"encoded value matches" ~expected ~actual string
  in
  let test_decode (input, expected) () =
    let actual = of_string input in
    check' ~msg:"decoded value matches" ~expected ~actual testable
  in
  tests
  |> List.mapi (fun i data ->
    [ test_case (Printf.sprintf "%s encode %d" kind i) `Quick @@ test_encode data
    ; test_case (Printf.sprintf "%s decode %d" kind i) `Quick @@ test_decode data
    ])
  |> List.flatten


let rpc_equal (a : Rpc.t) (b : Rpc.t) =
  match a, b with
  | Float f1, Float f2 -> Float.equal f1 f2 (* works on [Float.nan] *)
  | _ -> a = b


let pp_rpc = Fmt.using Rpc.to_string Fmt.string
let rpc = testable pp_rpc rpc_equal
let pp_call = Fmt.using Rpc.string_of_call Fmt.string
let call = testable pp_call ( = )
let pp_response = Fmt.using Rpc.string_of_response Fmt.string
let response = testable pp_response ( = )

let calls =
  [ "((4:name3:foo)(6:params())(15:is_notification5:false))", Rpc.call "foo" []
  ; ( "((4:name13:Something.bar)(6:params((6:String1:x)(3:Int1:4)))(15:is_notification5:false))"
    , Rpc.call "Something.bar" [ Rpc.String "x"; Rpc.Int 4L ] )
  ]


let responses =
  [ ( "((7:success4:true)(8:contents4:Null)(15:is_notification5:false))"
    , Rpc.{ success = true; contents = Null; is_notification = false } )
  ; ( "((7:success4:true)(8:contents(4:Enum((4:Bool4:true)(4:Bool5:false))))(15:is_notification5:false))"
    , Rpc.
        { success = true
        ; contents = Rpc.Enum [ Rpc.Bool true; Rpc.Bool false ]
        ; is_notification = false
        } )
  ]


let tests =
  List.flatten
    [ test_roundtrip "t" tests rpc Csexprpc.to_string Csexprpc.of_string
    ; test_roundtrip "call" calls call Csexprpc.string_of_call Csexprpc.call_of_string
    ; test_roundtrip
        "responses"
        responses
        response
        Csexprpc.string_of_response
        Csexprpc.response_of_string
    ]
