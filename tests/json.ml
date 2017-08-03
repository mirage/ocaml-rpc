let bad = "
 { foo\"a\":\"b\" }
"
let good = "
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

let v1_bad_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": \"0\"
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

let v2_bad_id = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": \"0\",
	\"jsonrpc\": \"2.0\"
}"

let v2_bad_jsonrpc = "{
	\"method\": \"session.login_with_password\",
	\"params\": [\"user\", \"password\"],
	\"id\": 0,
	\"jsonrpc\": \"1.0\"
}"

let tests1 = [
  "bad", bad, false;
  "good", good, true;
]

let tests2 = [
  "v1", v1, true;
  "v1_null_id", v1_null_id, false; (*stricter than the specs*)
  "v1_bad_id", v1_bad_id, false;
  "v1_no_id", v1_no_id, false;
  "v1_no_params", v1_no_params, false;
  "v2", v2, true;
  "v2_no_params", v2_no_params, false; (*stricter than the specs*)
  "v2_null_id", v2_null_id, false; (*stricter than the specs*)
  "v2_no_method", v2_no_method, false;
  "v2_bad_id", v2_bad_id, false;
  "v2_bad_jsonrpc", v2_bad_jsonrpc, false;
]

let handle_exception test_name e pass =
  if pass then
    (Printf.printf "Failed: test %s was not supposed to throw a parse failure\n" test_name;
    false)
  else
    (Printf.printf "Passed: test %s threw %s\n" test_name (Printexc.to_string e);
    true)


let parse_json (test_name, json, pass) =
  try
    let _ = Jsonrpc.of_string json in
    Printf.printf "Passed: test %s\n" test_name;
    true;
  with e ->
    handle_exception test_name e pass

let call_of_json (test_name, json, pass) =
  try
    let _ = Jsonrpc.call_of_string json in
    Printf.printf "Passed: test %s\n" test_name;
    true
  with e ->
    handle_exception test_name e pass

let _ =
  let results1 = tests1 |> List.map parse_json in
  let results2 = tests2 |> List.map call_of_json in
  let failures = results1 @ results2 |> List.filter (fun x -> not x) in
  match failures with
  | [] -> ()
  | _ -> failwith (Printf.sprintf "Test failures: %d"  (List.length failures))
