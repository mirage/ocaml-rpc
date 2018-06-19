let invalid_json = "
 { foo\"a\":\"b\" }
"
let valid_json = "
 { \"a\":\"b\" }
"

let valid_json_with_junk = "
 { \"a\":\"b\" }Some junk
"

let v1 = "{
  \"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": 0
}"

let v1_null_id = "{
  \"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": null
}"

let v1_string_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": \"0\"
}"

let v1_bad_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": [3]
}"

let v1_no_params ="{
	\"method\": \"session.login_with_password\",
	\"id\": \"0\"
}"

let v1_no_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"]
}"

let v2 ="{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": 0,
	\"jsonrpc\": \"2.0\"
}"

let v2_no_params = "{
	\"method\": \"session.login_with_password\",
	\"id\": 0,
	\"jsonrpc\": \"2.0\"
}"

let v2_null_id = "{
	\"method\": \"session.login_with_password\",
  \"params\": [\"user\", \"password\"],
  \"id\": null,
	\"jsonrpc\": \"2.0\"
}"

let v2_no_method = "{
	\"params\": [\"user\", \"password\"],
	\"id\": 0,
	\"jsonrpc\": \"2.0\"
}"

let v2_string_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": \"0\",
	\"jsonrpc\": \"2.0\"
}"

let v2_bad_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": \"0\",
	\"jsonrpc\": [2]
}"

let v2_bad_jsonrpc = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": 0,
	\"jsonrpc\": \"1.0\"
}"

let v2_mixed = "{
    \"jsonrpc\": \"2.0\",
    \"result\": \"OpaqueRef:0d01bcdd-9b33-a0d8-1870-d4fb80af354e\",
    \"error\": null,
    \"id\": 0
}"

let v2_success = "{
    \"jsonrpc\": \"2.0\",
    \"result\": \"OpaqueRef:0d01bcdd-9b33-a0d8-1870-d4fb80af354e\",
    \"id\": 0
}"

let v2_success_with_junk = "{
    \"jsonrpc\": \"2.0\",
    \"result\": \"OpaqueRef:0d01bcdd-9b33-a0d8-1870-d4fb80af354e\",
    \"id\": 0
}{!&some junk for test"

let v2_failure_bad_error = "{
    \"jsonrpc\": \"2.0\",
    \"error\": [ \"SESSION_AUTHENTICATION_FAILED\", \"root\", \"Authentication failure\" ],
    \"id\": 0
}"

let v2_failure = "{
    \"jsonrpc\": \"2.0\",
    \"error\": { \"code\": 1, \"message\": \"foo\", \"data\": \"bar\" },
    \"id\": 0
}"

let v2_failure_no_data = "{
    \"jsonrpc\": \"2.0\",
    \"error\": { \"code\": 1, \"message\": \"foo\" },
    \"id\": 0
}"

let v2_failure_bad_code = "{
    \"jsonrpc\": \"2.0\",
    \"error\": { \"code\": \"a\", \"message\": \"foo\" },
    \"id\": 0
}"

let v2_failure_bad_message = "{
    \"jsonrpc\": \"2.0\",
    \"error\": { \"code\": 1, \"message\": 2 },
    \"id\": 0
}"

let tests_json = [
  "invalid_json", invalid_json, false;
  "valid_json", valid_json, true;
  "valid_json_with_junk", valid_json_with_junk, false;
]

let tests_json_plus = [
  "valid_json_with_junk", valid_json_with_junk, true;
]

let tests_call = [
  "v1", v1, true;
  "v1_null_id", v1_null_id, false; (*stricter than the specs*)
  "v1_string_id", v1_string_id, true;
  "v1_bad_id", v1_bad_id, false; (*stricter than the specs*)
  "v1_no_id", v1_no_id, false;
  "v1_no_params", v1_no_params, false;
  "v2", v2, true;
  "v2_no_params", v2_no_params, true;
  "v2_null_id", v2_null_id, false; (*stricter than the specs*)
  "v2_no_method", v2_no_method, false;
  "v2_string_id", v2_string_id, true;
  "v2_bad_id", v2_bad_id, false;
  "v2_bad_jsonrpc", v2_bad_jsonrpc, false;
]

let tests_response = [
  "v2_success", v2_success, true;
  "v2_success_with_junk", v2_success_with_junk, false;
  "v2_failure", v2_failure, false;
  "v2_mixed", v2_mixed, false;
  "v2_failure_bad_error", v2_failure_bad_error, false;
  "v2_failure_no_data", v2_failure_no_data, true;
  "v2_failure_bad_code", v2_failure_bad_code, false;
  "v2_failure_bad_message", v2_failure_bad_message, false;
]

let tests_response_plus = [
  "v2_success_with_junk", v2_success_with_junk, true;
]

let invoke parse_func (test_name, json, pass) =
  let run () = parse_func json |> ignore in
  if pass then
    Alcotest.(check unit) test_name () (run ())
  else
    try
      run ();
      Alcotest.failf "test %s did not fail" test_name
    with e -> Printf.printf "test %s failed as expected: %s\n" test_name (Printexc.to_string e)

let test tcs unmarshal () =
  List.iter (invoke unmarshal) tcs

let tests =
  [ "Jsonrpc.of_string", `Quick, test tests_json Jsonrpc.of_string
  ; "Jsonrpc.of_string ~strict:false", `Quick, test tests_json_plus (Jsonrpc.of_string ~strict:false)
  ; "Jsonrpc.call_of_string", `Quick, test tests_call Jsonrpc.call_of_string
  ; "Jsonrpc.response_of_string", `Quick, test tests_response Jsonrpc.response_of_string
  ; "Jsonrpc.response_of_string ~strict:false", `Quick, test tests_response_plus (Jsonrpc.response_of_string ~strict:false)
  ]
