let invalid_json = "
 { foo\"a\":\"b\" }
"
let valid_json = "
 { \"a\":\"b\" }
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

let tests1 = [
  "invalid_json", invalid_json, false;
  "valid_json", valid_json, true;
]

let tests2 = [
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

let tests3 = [
  "v2_success", v2_success, true;
  "v2_failure", v2_failure, false;
  "v2_mixed", v2_mixed, false;
  "v2_failure_bad_error", v2_failure_bad_error, false;
  "v2_failure_no_data", v2_failure_no_data, true;
  "v2_failure_bad_code", v2_failure_bad_code, false;
  "v2_failure_bad_message", v2_failure_bad_message, false;
]

let invoke parse_func (test_name, json, pass) =
  try
    let _ = parse_func json in
    Printf.printf "Passed: test %s\n" test_name;
    true;
  with e ->
    if pass then
      (Printf.printf "Failed: test %s was not supposed to throw a parse failure\n" test_name;
      false)
    else
      (Printf.printf "Passed: test %s threw %s\n" test_name (Printexc.to_string e);
      true)

let _ =
  let results1 = tests1 |> List.map (fun x -> invoke Jsonrpc.of_string x) in
  let results2 = tests2 |> List.map (fun x -> invoke Jsonrpc.call_of_string x) in
  let results3 = tests3 |> List.map (fun x -> invoke Jsonrpc.response_of_string x) in
  let failures = results1 @ results2 @ results3 |> List.filter (fun x -> not x) in
  match failures with
  | [] -> ()
  | _ -> failwith (Printf.sprintf "Test failures: %d"  (List.length failures))
